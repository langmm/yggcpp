import os
import sys
import argparse
import subprocess
import shutil
import ctypes
import warnings
from functools import cached_property
from collections import OrderedDict


_tool_registry = {}
_platform = None
_library_ext = None
if sys.platform == 'darwin':
    _platform = 'osx'
    _library_ext = '.dylib'
elif 'linux' in sys.platform:
    _platform = 'linux'
    _library_ext = '.so'
elif sys.platform in ['win32', 'cygwin']:
    _platform = 'win'
    _library_ext = '.dll'


class ToolMeta(type):

    def __new__(meta, name, bases, class_dict):
        cls = type.__new__(meta, name, bases, class_dict)
        if cls.name is not None:
            _tool_registry.setdefault(cls.name, cls)
        return cls


def select_tool(mode='dependencies'):
    if _platform == 'osx':
        tool = 'otool'
    elif _platform == 'linux':
        if mode == 'objects':
            tool = 'objdump'
        else:
            tool = 'ldd'
    elif _platform == 'win':
        if mode == 'objects':
            tool = 'objconv'
        else:
            tool = 'dumpbin'
    else:
        raise RuntimeError(f'Could not determine a tool for '
                           f'platform \"${_platform}\"')
    return tool


class SearchResult:
    r"""Class for storing search results."""

    def __init__(self, name, path=None, method=None, children=None,
                 depth=0):
        if isinstance(name, list):
            assert all([path is None, method is None, children is None])
            children = OrderedDict([(x.name, x) for x in children])
            name = None
        if children is None:
            children = OrderedDict()
        self.name = name
        self.path = path
        self.method = method
        self.children = children
        self.depth = depth

    def find(self, x):
        if x == self.name:
            return self
        return self.children.get(x, None)

    def padding(self):
        key_len = len(self.name) if self.name else 0
        if not self.children:
            return key_len
        return max(key_len, max(x.padding() for x in
                                self.children.values()))

    def format(self, indent=1, pad=None, return_lines=False):
        if pad is None:
            pad = self.padding() + 4
        out = []
        if self.name:
            out.append(
                self.name + ((pad - len(self.name)) * ' ')
                + str(self.path)
                + (f' [{self.method}]' if self.method else '')
                + (f' - {self.depth}' if self.depth > 0 else '')
            )
        for x in self.children.values():
            out += [
                (indent * ' ') + iline for iline in
                x.format(indent=indent, pad=pad, return_lines=True)
            ]
        if return_lines:
            return out
        return '\n'.join(out)


class ToolBase(metaclass=ToolMeta):
    r"""Base class for inspection tools."""

    name = None
    search_paths = ['PATH', 'LD_LIBRARY_PATH', 'DYLD_LIBRARY_PATH',
                    'CONDA_PREFIX']
    hsep = 80 * '='
    hsepn = (80 * '=') + '\n'
    max_depth = 5
    object_flags = {}

    def __init__(self, target, cmake_runtimes=None, recurse=False,
                 verbose=False, depth=0):
        self.target = target
        self.cmake_runtimes = cmake_runtimes
        self.recurse = recurse
        self.verbose = verbose
        self.depth = depth
        if os.path.isfile(self.target):
            self.target = os.path.abspath(self.target)
        else:
            self.target = self.search(self.target)
            if self.target.path is None:
                raise ValueError(
                    f"Could not find target \"{self.target.name}\"")
            self.target = self.target.path

    @classmethod
    def which(cls):
        return shutil.which(cls.name)

    @classmethod
    def _run(cls, cmd):
        return subprocess.run(
            cmd, capture_output=True, shell=True, check=True,
        ).stdout.decode('utf-8')

    @cached_property
    def formatted_runtime_libraries(self):
        out = (
            f'{self.hsep}\nRuntime dependencies for {self.target}\n'
            f'{self.hsep}\n'
        )
        out += '\n'.join(self.runtime_libraries)
        return out

    @cached_property
    def formatted_search_paths(self):
        out = (
            f'{self.hsep}\nSearch PATHS for {self.target}\n{self.hsep}\n'
        )
        for path in self.search_paths:
            if path not in os.environ:
                continue
            out += self.format_search_path(path)
        return out

    def format_search_path(self, x, indent=(4 * ' ')):
        out = x
        value = os.environ[x].split(os.pathsep)
        if value:
            out += f'\n{indent}' + f'\n{indent}'.join(value)
        return out

    @cached_property
    def formatted_search_results(self):
        out = (
            f'{self.hsep}\nDependency locations for {self.target}\n'
            f'{self.hsep}\n'
        )
        out += self.search_results.format()
        return out

    @cached_property
    def formatted_object_contents(self):
        out = (
            f'{self.hsep}\nObject contents for {self.target}\n'
            f'{self.hsep}\n'
        )
        for k, v in self.object_contents.items():
            out += f"\n{k}\n{v}\n"
        return out

    @cached_property
    def search_results(self):
        out = SearchResult(self.target, self.target)
        try:
            self.add_children(out, root=out)
        except RecursionError:
            pass
        return out

    @cached_property
    def runtime_libraries(self):
        if ' ' in self.target:
            cmd = self.command(f"\"{self.target}\"")
        else:
            cmd = self.command(self.target)
        try:
            raw_output = self._run(cmd)
            return [x for x in self.extract_libraries(raw_output) if x]
        except subprocess.CalledProcessError as e:
            warnings.warn(f"Error running {cmd}: {e}. "
                          f"output={e.output}, "
                          f"stdout={e.stdout}, stderr={e.stderr}")
            return []

    @cached_property
    def object_contents(self):
        out = {}
        for method in ["header", "sections"]:
            if ' ' in self.target:
                cmd = self.object_command(f"\"{self.target}\"", method)
            else:
                cmd = self.object_command(self.target, method)
            try:
                out[method] = self._run(cmd)
            except subprocess.CalledProcessError as e:
                warnings.warn(f"Error running {cmd}: {e}. "
                              f"output={e.output}, "
                              f"stdout={e.stdout}, stderr={e.stderr}")
        return out

    def _create_child(self, *args, **kwargs):
        kwargs.setdefault('verbose', self.verbose)
        kwargs.setdefault('recurse', self.recurse)
        kwargs.setdefault('depth', self.depth + 1)
        return type(self)(*args, **kwargs)

    def add_children(self, out, root=None, depth=0):
        # print(out.name, self.runtime_libraries)
        if root is None:
            root = out
        out.depth = self.depth
        for xx in self.runtime_libraries:
            if xx == out.name:
                continue
            if depth > 0 and root.find(xx):
                out.children[xx] = SearchResult(xx, 'RECURSIVE')
            else:
                out.children[xx] = self.search(xx)
            out.children[xx].depth = self.depth + 1
        if self.verbose:
            print(f"{self.hsep}\nPARTIAL SEARCH:\n{out.format()}")
        if self.recurse and self.depth < self.max_depth:
            for x in out.children.values():
                if not (x.path and os.path.isfile(x.path)):
                    continue
                tool = self._create_child(x.path)
                tool.add_children(x, root=root)

    def search(self, x):
        for path in self.search_paths:
            if path not in os.environ:
                continue
            path_value = os.environ[path]
            if path == 'CONDA_PREFIX':
                prefix = path_value
                path_value = [os.path.join(prefix, 'bin')]
                if _platform == 'win':
                    path_value.append(
                        os.path.join(prefix, 'Library', 'bin'))
                path_value = os.pathsep.join(path_value)
            out = shutil.which(x, path=path_value, mode=os.F_OK)
            if out is not None:
                return SearchResult(x, out, method=path)
        for path in self.search_paths:
            for d in path_value.split(os.pathsep):
                ptry = os.path.join(d, x)
                if os.path.isfile(ptry):
                    return SearchResult(x, ptry, method=f'{path}-DIRECT')
        if _library_ext in x:
            try:
                ctypes.CDLL(x)
                return SearchResult(x, x, method='VIRTUAL')
            except OSError:
                pass
        if self.cmake_runtimes:
            for cmake_runtime in self.cmake_runtimes:
                if cmake_runtime.endswith(x):
                    return SearchResult(x, cmake_runtime,
                                        method='CMAKE RUNTIME')
        if os.path.isfile(x):
            return SearchResult(x, os.path.abspath(x),
                                method='CURRENT DIRECTORY')
        return SearchResult(x)

    @classmethod
    def object_command(cls, target, method):
        if method in cls.object_flags:
            return f"{cls.name} {cls.object_flags[method]} {target}"
        raise NotImplementedError

    @classmethod
    def command(cls, target):
        raise NotImplementedError

    @classmethod
    def extract_libraries(cls, raw_output):
        raise NotImplementedError


class ObjconvTool(ToolBase):

    name = 'objconv'
    object_flags = {
        'header': '-df',
        'sections': '-dh',
        'symbols': '-ds',
        'relocation': '-dr',
        'strings': '-dn',
    }
        

class ObjdumpTool(ToolBase):

    name = 'objdump'

    @classmethod
    def command(cls, target):
        return f"objdump -x {target}"

    @classmethod
    def extract_libraries(cls, raw_output):
        return [
            x.split("DLL Name:")[-1].strip() for x in
            raw_output.splitlines() if "DLL Name:" in x
        ]


class DumpbinTool(ToolBase):

    name = 'dumpbin'

    @classmethod
    def command(cls, target):
        if ' ' in target:
            target = f'"{target}"'
        return f"dumpbin /dependents {target}"

    @classmethod
    def extract_libraries(cls, raw_output):
        if 'dependencies:' not in raw_output:
            return []
        return [
            x.strip() for x in
            raw_output.split('dependencies:')[-1].split(
                'Summary')[0].splitlines()
        ]


class OtoolTool(ToolBase):

    name = 'otool'
    object_flags = {
        'header': '-h',
        'sections': '-f',
        'symbols': '-d',
        'relocation': '-r',
        'strings': '-t',
    }

    @classmethod
    def command(cls, target):
        return f"otool -L {target}"

    @classmethod
    def extract_libraries(cls, raw_output):
        return [
            x.split('(')[0].strip() for x in
            raw_output.split(':', 1)[-1].splitlines()
        ]

    def search(self, x, **kwargs):
        if '@rpath/' in x:
            rpaths = self.rpaths
            for rpath in rpaths:
                xalt = x.replace('@rpath', rpath)
                if os.path.isfile(xalt):
                    return SearchResult(x, xalt)
            print(f"Failed to resolve rpath: {x} "
                  f"(rpaths = {rpaths})")
            return self.search(x.replace('@rpath/', ''), **kwargs)
        return super(OtoolTool, self).search(x, **kwargs)

    @cached_property
    def rpaths(self):
        result = self._run(f'otool -l {self.target}')
        commands = result.split('Load command')
        out = [os.path.dirname(self.target)]
        for x in commands:
            if 'LC_RPATH' not in x:
                continue
            out.append(
                x.rsplit('path ', 1)[-1].split('(', 1)[0].strip()
            )
        return out


class LddTool(ToolBase):

    name = 'ldd'

    @classmethod
    def command(cls, target):
        return f"ldd {target}"

    @classmethod
    def extract_libraries(cls, raw_output):
        out = [
            x.split('(')[0].strip() for x in
            raw_output.splitlines()
        ]
        out = [
            x.split('=>')[-1].strip() if '=>' in x else x
            for x in out
        ]
        return out


def inspect(args):
    if not args.tool:
        args.tool = select_tool(mode=args.mode)
    out = {}
    for target in args.target:
        print(f"TARGET: {target}")
        tool = _tool_registry[args.tool](
            target, cmake_runtimes=args.cmake_runtimes,
            recurse=args.recurse, verbose=args.verbose,
        )
        print(f'{tool.name}: {tool.which()}')
        if args.mode == "dependencies":
            print(tool.formatted_runtime_libraries)
            print(tool.formatted_search_paths)
            print(tool.formatted_search_results)
            out[target] = tool.search_results
        elif args.mode == "objects":
            print(tool.formatted_object_contents)
            out[target] = tool.object_contents
        else:
            raise NotImplementedError(args.mode)
    return out


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Locate runtime dependencies")
    parser.add_argument(
        "target", type=str, nargs="+",
        help=("Path to one or more object files, executables or "
              "libraries that should be inspected"),
    )
    parser.add_argument(
        "--tool", type=str, choices=sorted(list(_tool_registry.keys())),
        help=("Name of the tool that should be used to extract "
              "runtime library dependencies from an executable or "
              "dynamic/shared library")
    )
    parser.add_argument(
        "--mode", type=str, default="dependencies",
        choices=["dependencies", "objects"],
        help="Inspection mode",
    )
    parser.add_argument(
        "--cmake-runtimes", nargs='*', type=str,
        help=("Result of TARGET_RUNTIME_DLLS cmake generator expression "
              "for the target"),
    )
    parser.add_argument(
        "--recurse", action="store_true",
        help="Show runtime dependencies of runtime dependencies",
    )
    parser.add_argument(
        "--verbose", action="store_true",
        help="Show runtime dependencies as they are added",
    )
    args = parser.parse_args()
    inspect(args)
