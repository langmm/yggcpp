import os
import sys
import argparse
import subprocess
import shutil
import ctypes
from functools import cached_property


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


class ToolBase(metaclass=ToolMeta):
    r"""Base class for inspection tools."""

    name = None

    def __init__(self, target):
        self.target = os.path.abspath(target)

    @classmethod
    def _run(cls, cmd):
        return subprocess.run(
            cmd, capture_output=True, shell=True, check=True,
        ).stdout.decode('utf-8')

    @cached_property
    def runtime_libraries(self):
        cmd = self.command(self.target)
        raw_output = self._run(cmd)
        return [x for x in self.extract_libraries(raw_output) if x]

    def search(self, x):
        if os.path.isfile(x):
            return x
        paths = ['PATH', 'LD_LIBRARY_PATH', 'DYLD_LIBRARY_PATH']
        for path in paths:
            if path not in os.environ:
                continue
            out = shutil.which(x, path=os.environ[path])
            if out is not None:
                return out
        if _library_ext in x:
            try:
                ctypes.CDLL(x)
                out = x + ' [VIRTUAL]'
            except OSError:
                pass
        return out

    @classmethod
    def command(cls, target):
        raise NotImplementedError

    @classmethod
    def extract_libraries(cls, raw_output):
        raise NotImplementedError


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
        return f"dumpbin /dependents {target}"

    @classmethod
    def extract_libraries(cls, raw_output):
        return [
            x.strip() for x in
            raw_output.split('dependencies:')[-1].split(
                'Summary')[0].splitlines()
        ]


class OtoolTool(ToolBase):

    name = 'otool'

    @classmethod
    def command(cls, target):
        return f"otool -L {target}"

    @classmethod
    def extract_libraries(cls, raw_output):
        return [
            x.split('(')[0].strip() for x in
            raw_output.split(':', 1)[-1].splitlines()
        ]

    def search(self, x):
        rpaths = self.rpaths
        if '@rpath/' in x:
            for rpath in rpaths:
                xalt = x.replace('@rpath', rpath)
                if os.path.isfile(xalt):
                    return xalt
            print(f"Failed to resolve rpath: {x} "
                  f"(rpaths = {rpaths})")
            return self.search(x.replace('@rpath/', ''))
        return super(OtoolTool, self).search(x)

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
        if _platform == 'osx':
            args.tool = 'otool'
        elif _platform == 'linux':
            args.tool = 'ldd'
        elif _platform == 'win':
            args.tool = 'dumpbin'
    tool = _tool_registry[args.tool](args.target)
    deps = tool.runtime_libraries
    hsep = 80 * '=' + '\n'
    print(f'{hsep}Runtime dependencies for {tool.target}\n{hsep}'
          + '\n'.join(deps))
    out = {x: tool.search(x) for x in deps}
    key_len = len(max(out.keys(), key=len)) + 4
    message = []
    for k, v in out.items():
        message.append(k + ((key_len - len(k)) * ' ') + str(v))
    print(f'{hsep}Dependency locations for {tool.target}\n{hsep}'
          + '\n'.join(message))
    return out


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Locate runtime dependencies")
    parser.add_argument(
        "target", type=str,
        help="Path to executable or library that should be inspected",
    )
    parser.add_argument(
        "--tool", type=str, choices=sorted(list(_tool_registry.keys())),
        help=("Name of the tool that should be used to extract "
              "runtime library dependencies from an executable or "
              "dynamic/shared library")
    )
    args = parser.parse_args()
    inspect(args)
