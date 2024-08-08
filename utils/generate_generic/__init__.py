# TODO
# - Handling of encoding in bytes/unicode
# - handle special case of display
# - convert logging
# - change to map as default w/ object as alias?
# - fix excluded functions
import os
import re
import copy
import pprint
import itertools
from collections import OrderedDict
from generate_generic.base import get_file_unit, camel2underscored
_base_dir = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))


class GeneratedFile(object):
    r"""Base class for generating files."""

    generated_flag = ('LINES AFTER THIS WERE GENERATED AND SHOULD NOT '
                      'BE MODIFIED DIRECTLY')
    indent_append = ''
    file_suffix = ''

    def __init__(self, src, added=None, prefix_lines=None,
                 suffix_lines=None):
        self.src = os.path.join(_base_dir, src)
        self.file_unit = get_file_unit(src)
        self.added = added
        if self.added is None:
            self.added = {}
        self.flag = ('\n' + self.indent_append + self.comment + ' '
                     + self.generated_flag)
        self.contents = ''
        self.prev_contents = ''
        if os.path.isfile(self.src):
            with open(self.src, 'r') as fd:
                self.prev_contents = fd.read()
                self.contents = self.prev_contents.split(self.flag)[0]
        self.lines = []
        self.prefix_lines = prefix_lines
        self.suffix_lines = suffix_lines

    @property
    def indent(self):
        return self.file_unit.indent

    @property
    def comment(self):
        return self.file_unit.comment

    def append(self, line):
        if not line:
            return
        if len(self.indent_append) > 0:
            line = (
                self.indent_append
                + line.replace('\n', '\n' + self.indent_append))
        if line in self.lines:
            print("DUPLICATE")
            print(line)
        assert line not in self.lines
        if line not in self.lines:
            self.lines.append(line)

    def write(self, debug=False, verbose=False):
        prefix_lines = [
            self.flag, self.indent_append + self.comment + 68 * '=']
        if self.prefix_lines:
            prefix_lines += self.prefix_lines
        nprefix = len(self.lines)
        self.lines = prefix_lines + self.lines
        new_content = '\n'.join(self.lines) + self.file_suffix
        if self.suffix_lines:
            new_content += '\n' + '\n'.join(self.suffix_lines)
        if debug:
            if len(self.lines) == nprefix:
                raise Exception(self.src)
        if debug or verbose:
            print(f"\n{self.src}{new_content}")
        if (not debug) and len(self.lines) > nprefix:
            with open(self.src, 'w') as fd:
                fd.write(self.contents + new_content)
        for v in self.added.values():
            if isinstance(v, GeneratedFile):
                v.write(debug=debug, verbose=verbose)

    def generate(self, dont_write=False, **kwargs):
        if not dont_write:
            self.write(**kwargs)

    def wrap_file(self, src, **kwargs):
        if isinstance(src, str):
            src = self.file_unit.parse_file(src, **kwargs)
        self.lines += self.file_unit.from_unit(src).format().splitlines()

    def parse(self, **kwargs):
        kwargs.setdefault('name', self.src)
        kwargs.setdefault('contents', self.prev_contents)
        return self.file_unit.parse_file(**kwargs)

    def test_parse_wrap(self, **kwargs):
        xmed = self.parse(check_format=True, **kwargs)
        lines = xmed.format()
        xalt = self.parse(name=self.src, contents=lines, **kwargs)
        assert xalt == xmed


class AmendedFile(GeneratedFile):

    regexes = {}
    types = {}
    scalar_varients = {}
    cpp_types = {}
    idx_types = {}
    idx = {}
    names = {}
    methods = {}
    param = {}
    try_begin = 'TRY_BEGIN'
    try_end = 'TRY_END'
    file_suffix = ''
    preserved_param = ['xdocs', 'xdef', 'xbody', 'xfunc']
    param_order = ['name', 'type', 'ret_type', 'cpp_type', 'default',
                   'init_default', 'function', 'set_shape', 'set',
                   'args_docs', 'docs']
    aliased_functions = {}
    excluded_functions = []

    def __init__(self, language, *args, **kwargs):
        self.init_param()
        self.language = language
        super(AmendedFile, self).__init__(*args, **kwargs)

    def init_param(self):
        self.types = copy.deepcopy(self.types)
        self.types['all'] = {}
        for k, v in self.types.items():
            if k == 'all':
                continue
            self.types['all'].update(v)
        for k, v in list(self.types.items()):
            self.types[f'inv_{k}'] = {}
            for kk, vv in v.items():
                if ((kk not in self.types.get('no_inverse', {})
                     and vv not in self.types[f'inv_{k}'])):
                    self.types[f'inv_{k}'][vv] = kk

    @classmethod
    def format_dict(self, x, order=None, preserved=None, check=None):
        if order is None:
            order = []
        order += [k for k in x.keys() if k not in order]
        for k in order:
            if preserved and k in preserved:
                continue
            if isinstance(x.get(k, None), str):
                try:
                    x[k] = x[k].format(**x)
                except KeyError:
                    pass
        if check:
            for k, v in x.items():
                if ((isinstance(v, str)
                     and any(f'{{{kk}}}' in v for kk in check))):
                    pprint.pprint(x)
                    raise AssertionError(
                        f"A format parameter is still present in '{v}'"
                        f"(checked {check})")

    def append(self, line):
        if line:
            for k in ['type', 'ret_type', 'X']:
                if f'{{{k}}}' in line:
                    print(line)
                assert f'{{{k}}}' not in line
        return super(AmendedFile, self).append(line)

    def generate(self, **kwargs):
        iterover = OrderedDict([
            ('container', [None] + list(self.types['container'].keys())),
            ('action', [k for k in self.methods.keys() if k]),
            ('raw_type', list(self.types['all'].keys())),
        ])
        for it in itertools.product(*iterover.values()):
            kws = {k: v for k, v in zip(iterover.keys(), it)}
            if ((kws['raw_type'] in self.types['scalar']
                 or kws['raw_type'] in self.types['util']
                 or (kws['raw_type'] == 'item_nbytes'
                     and kws['action'].startswith('set')))):
                continue
            self.generate_method(**kws)
            if kws['raw_type'] in self.types['varient']:
                for k in self.types['scalar'].keys():
                    for v in self.scalar_varients[k]:
                        self.generate_method(scalar_raw_type=k,
                                             X=v, **kws)
        self.generate_long_double()
        return super(AmendedFile, self).generate(**kwargs)

    def generate_long_double(self, **kwargs):
        iterover = OrderedDict([
            ('container', [None] + list(self.types['container'].keys())),
            ('action', [k for k in self.methods.keys() if k]),
            ('raw_type', list(self.types['varient'].keys())),
        ])
        for it in itertools.product(*iterover.values()):
            kws = {k: v for k, v in zip(iterover.keys(), it)}
            for k in ['float', 'complex']:
                self.generate_method(scalar_raw_type=k, X='long double',
                                     **kws)

    def type_info(self, tname, prefix=None):
        out = {
            'docs_name': tname,
            'name': self.names.get(tname, tname),
            'type': self.types['all'][tname],
            'cpp_type': self.cpp_types.get(
                tname, self.types['all'][tname]),
        }
        if tname in self.types['geometry']:
            out['geom_name'] = out['cpp_type'].split('::')[-1]
        if prefix:
            out = {f'{prefix}{k}': v for k, v in out.items()}
        return out

    def generate_method(self, **kwargs):
        is_valid = self.methods[kwargs['action']].get(
            'is_valid', lambda x: True)
        if not is_valid(kwargs):
            return
        kwargs.update(self.type_info(kwargs['raw_type']))
        if kwargs.get('scalar_raw_type', None):
            kwargs.update(self.type_info(kwargs['scalar_raw_type'],
                                         prefix='scalar_'))
            kwargs['docs_name'] = (
                kwargs['scalar_raw_type'] + ' ' + kwargs['docs_name'])
        elif kwargs['raw_type'] in self.types['varient']:
            kwargs['name'] = kwargs['raw_type']
        kwargs.setdefault('scalar_name', '')
        kwargs.update({
            'generic_t': self.types['generic']['any'],
            'suffix': '',
        })
        self.get_param(kwargs)
        self.add_method(copy.deepcopy(kwargs))

    def kws2tuple(self, kwargs):
        out = [kwargs['raw_type'], kwargs.get('container', None),
               kwargs.get('array_container', None),
               kwargs.get('X', None)]
        return tuple(out)

    def add_method(self, kwargs, **kws):
        kwargs.setdefault('basefile', self)
        if 'X' in kwargs:
            kwargs['X_name'] = kwargs['X'].replace(' ', '_')
            if kwargs['X'].isdigit():
                kwargs['X_bytes'] = str(int(int(kwargs['X']) / 8))
            elif kwargs['X'] == 'float':
                kwargs['X_bytes'] = '4'
            elif kwargs['X'] == 'double':
                kwargs['X_bytes'] = '8'
            elif kwargs['X'] == 'long double':
                kwargs['X_bytes'] = '16'
            # else:
            #     kwargs['X_bytes'] = kwargs['X']
        self.format_dict(
            kwargs, preserved=self.preserved_param,
            order=([f'scalar_{kk}' for kk in self.param_order]
                   + self.param_order),
            check=['X', 'X_name', 'X_bytes'])
        for k in ['name', 'scalar_name']:
            if k in kwargs:
                kwargs[f'{k}_len'] = len(kwargs[k])
        self.format_dict(
            kwargs, preserved=self.preserved_param,
            order=([f'scalar_{kk}' for kk in self.param_order]
                   + self.param_order),
            check=['X', 'X_name', 'X_bytes'])
        kwargs.setdefault('aliased_function', kwargs['function'])
        if kwargs['function'] in self.aliased_functions:
            kwargs['function'] = self.aliased_functions[kwargs['function']]
        # if kwargs['function'] in (kwargs['basefile'].excluded_functions
        #                           + self.excluded_functions):
        # if kwargs['function'] in self.excluded_functions:
        if kwargs['function'] in kwargs['basefile'].excluded_functions:
            return
        self.append(self.format(kwargs, **kws))

    def get_param(self, kwargs):
        key = kwargs['raw_type']
        if ((key in self.types['varient']
             and not kwargs.get('scalar_raw_type', None))):
            key = f'raw_{key}'

        def _get(subparam):
            if ((kwargs['raw_type'] == 'scalar'
                 and kwargs.get('scalar_raw_type', None) in subparam)):
                return subparam[kwargs['scalar_raw_type']]
            return subparam.get(key, subparam.get('default', ''))

        for k in ['args', 'args_def', 'args_docs', 'ret_type']:
            kwargs[k] = _get(self.param[k][kwargs['action']])
        for k in self.methods[kwargs['action']].get('required_param', []):
            kwargs[k] = _get(self.param[k])
        if kwargs.get('container', None):
            kwargs.update(
                container_name=f"{kwargs['container']}_",
                idx_type=self.idx_types[kwargs['container']],
                idx=self.idx[kwargs['container']])

    def format_docs(self, kwargs):
        if 'args_docs' in kwargs:
            kwargs['args_docs'] = kwargs['args_docs'].format(**kwargs)
        if (not kwargs.get('docs', None)) and kwargs.get('xdocs', None):
            kwargs['docs'] = kwargs['xdocs'].format(**kwargs)
        if 'docs' in kwargs:
            kwargs['docs'] = kwargs['docs'].strip()
        docs = ''
        if kwargs.get('docs', None):
            docs = '\n'.join([
                self.docs_c + x.strip() if x.startswith('@') else
                self.docs_c + self.indent + x.strip()
                for x in kwargs['docs'].splitlines()])
            docs = (
                f"{self.begin_doc_block}{docs}\n{self.end_doc_block}")
        return docs

    def format(self, kwargs):
        raise NotImplementedError


class CFile(AmendedFile):

    comment = '//'
    docs_c = ' * '
    begin_doc_block = '/**\n'
    end_doc_block = ' */\n'
    indent = '  '
    typedef = '{const}{base}{ptr}'
    argsdef = '{type} {name}'
    castdef = '({c_type}){name}'
    excluded_functions = ['generic_map_get_keys', 'display_dtype',
                          'yggOutput', 'yggInput',
                          'yggOutputType', 'yggInputType',
                          'ygg_recv_nolimit', 'comm_recv_realloc',
                          'yggRpcServerType_global',
                          'register_function',
                          '_register_function',
                          'init_comm', '_init_comm', 'init_comm_flags',
                          'set_comm_language',
                          'ncommSend', 'ncommRecv', 'ncommCall',
                          '_call_pointer']
    char_types = [
        'const char*', 'char*']
    regexes = {
        'var': (
            r'(\((?P<var_par>\*+)?)?(?P<name>\w+)(?(var_par)(?:\)))'
            r'(?P<shape>(?:\[.+?\])+)?'),
        'type': (
            r'(?P<const>const\s+)?(?P<base>(?:[\w ])+)'
            r'(?P<ptr_space1>\s+)?(?P<ptr>\*+)?'),
        'docs': (
            r'(?:(?:\/\*\!)|(?:\/\*\*))\s*'
            r'(?P<docs>(?:(?:\s*\*)?(?:[^\/]))*?)\*\/'),
        'declare_var': (
            r'(?P<type>{type})'
            r'(?P<ptr_space2>(?(ptr)(?(ptr_space1)(?:\s*)|(?:\s+))|(?:\s+)))'
            r'{var}'),
        'declare_var_bulk': (
            r'(?:const\s+)?(?:[\w ]*\w)'
            r'(?:(?:\s+)|(?:\s+\*+\s*)|(?:\s*\*+\s+))'
            r'(?:(?:\w+)|(?:\(\s*\w+\s*\)))'
            r'(?:\[.+?\])*'),
        'declare_args': (
            r'\s*{declare_var}\s*(?:(?:\,)|(?:(?:\)|(?:$))))'),
        'declare_func': (
            r'^(?:\s*{docs}\s+)?'
            r'(?:\s*static inline\s*)?'
            r'\s*(?:YGG_API\s+)?(?P<ret_type>{type})\s+(?P<function>\w+)'
            r'(?P<args_def>\('
            r'(?:\s*{declare_var_bulk}\s*(?:\,))*'
            r'(?:\s*(?:(?:{declare_var_bulk})|(?P<va_args>\.\.\.))\s*)?'
            r'\))\s*[;\{{]\s*$'),
    }
    types = {
        'core': {
            'null': 'void*',
            'boolean': 'bool',
            'integer': 'int',
            'number': 'double',
            'string': 'const char*',
        },
        'no_inverse': {
            'item': 'void*',
            'item_nbytes': 'int',
        },
        'container': {
            'array': 'generic_t',
            'object': 'generic_t',
        },
        'scalar': {
            'int': 'int{X}_t',
            'uint': 'uint{X}_t',
            'float': '{X}',
            'complex': 'complex_{X_name}_t',
            # 'bytes': 'const char*',  # 'bytes_t',
            # 'unicode': 'const char*',  # 'unicode_t',
        },
        'geometry': {
            'ply': 'ply_t',
            'obj': 'obj_t',
        },
        'python': {
            'class': 'python_t',
            'function': 'python_t',
            'instance': 'python_t',
        },
        'varient': {
            'scalar': 'void*',
            '1darray': 'void*',
            'ndarray': 'void*',
        },
        'generic': {
            'schema': 'generic_t',
            'any': 'generic_t',
        },
        'util': {
            'comm': 'comm_t',
            'dtype': 'dtype_t',
            'length': 'size_t',
            'shape': 'size_t*',
            'shape_realloc': 'size_t**',
            'flag': 'long',
            'generic_ref': 'generic_ref_t',
            'noret': 'void',
            'flexible_string': 'char*',
            'string_return': 'const char*',
            'string_realloc': 'char**',
            'string_array': 'const char**',
            'registered_function': 'c_function',
            'commflag': 'FLAG_TYPE',
            'pointer': 'void*',
            'timeout': 'int64_t',
        },
    }
    container_aliases = {
        'object': 'map',
    }
    cpp_types = {
        'complex': "std::complex<{X}>",
        'obj': 'rapidjson::ObjWavefront',
        'ply': 'rapidjson::Ply',
    }
    idx_types = {
        'array': 'const size_t',
        'object': 'const char*',
    }
    idx = {
        'array': 'index',
        'object': 'key',
    }
    names = {
        'boolean': 'bool',
        'float': "{X_name}",
        'int': 'int{X}',
        'uint': 'uint{X}',
        'complex': "complex_{X_name}",
        'scalar': '{scalar_name}',
        '1darray': '1darray_{scalar_name}',
        'ndarray': 'ndarray_{scalar_name}',
        'class': 'python_class',
        'function': 'python_function',
        'instance': 'python_instance',
    }
    scalar_varients = {
        'float': ['float', 'double'],
        'int': ['8', '16', '32', '64'],
        'uint': ['8', '16', '32', '64'],
        'complex': ['float', 'double'],
    }
    methods = {
        '': {
            'definition': (
                '{ret_type} {function}{suffix}({args_def})'),
            'body': (
                '{body_prefix}{return}{aliased_function}({args});\n'
                '{body_suffix}'),
        },
        'size': {
            'is_valid': lambda x: (
                (not x.get('container', None))
                and x['raw_type'] in CFile.types['container']),
            'docs': (
                '@brief Get the number of elements in a {name}\n'
                '@param[in] x generic_t Generic object that is presumed '
                'to contain a\n  {name}\n'
                '@returns size_t Number of elements in {name}'),
            'definition': (
                'size_t generic_{name}_get_size{suffix}'
                '({generic_t} x{args_def})'),
            'body': (
                'size_t out = 0;\n'
                'TRY_BEGIN'
                'if (!is_generic_init(x)) {{\n'
                '  YggLogError << "{function}: Generic object is not '
                'initialized" '
                '<< std::endl;\n'
                '  return out;\n'
                '}}\n'
                'rapidjson::Value* d = (rapidjson::Value*)(x.obj);\n'
                'if (!{check}) {{\n'
                '  YggLogError << "{function}: Generic object is not '
                '{name}{message}: " << '
                '(*d) << std::endl;\n'
                '  return out;\n'
                '}}\n'
                '{get_size};\n'
                'TRY_END'
                'return out;\n'
            ),
            'required_param': ['check', 'get_size', 'message'],
        },
        'set': {
            'is_valid': lambda x: (not x.get('container', None)),
            'docs': (
                '@brief Set a given generic item to a {docs_name}\n'
                '@param[in] x The generic item to set\n'
                '{args_docs}'
                '@returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on '
                'success\n'),
            'definition': (
                'int generic_set_{name}{suffix}({generic_t} '
                'x{args_def})'),
            'body': (
                'int out = GENERIC_ERROR_;\n'
                'TRY_BEGIN'
                'if (!is_generic_init(x)) {{\n'
                '  YggLogError << "Generic object is not initialized" '
                '<< std::endl;\n'
                '  return out;\n'
                '}}\n'
                'rapidjson::Value* d = (rapidjson::Value*)(x.obj);\n'
                '{set};\n'
                'out = GENERIC_SUCCESS_;\n'
                'TRY_END'
                'return out;\n'),
            'required_param': ['check', 'set', 'set_shape'],
        },
        'set_cont': {
            'is_valid': lambda x: bool(x.get('container', None)),
            'docs': (
                '@brief Set an element in a {container} to a {docs_name}\n'
                '@param[in] x {container} to set element in\n'
                '{args_docs}'
                '@returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on '
                'success\n'),
            'definition': (
                'int generic_{container}_set_{name}{suffix}('
                '{generic_t} x{args_def})'),
            'body': (
                'int out = GENERIC_ERROR_;\n'
                'TRY_BEGIN'
                '{generic_t} item = init_generic_null();\n'
                'if (generic_set_{name}(item{args}) != '
                'GENERIC_SUCCESS_) {{\n'
                '  return out;\n'
                '}}\n'
                'out = set_generic_{container}(x, {idx}, item);\n'
                'destroy_generic(&item);\n'
                'TRY_END'
                'return out;\n')
        },
        'get': {
            'docs': (
                '@brief Get a {docs_name} from a generic item\n'
                '@param[in] x Generic item to retrieve data from\n'
                '{args_docs}'),
            'is_valid': lambda x: (not x.get('container', None)),
            'definition': (
                '{ret_type} generic_get_{name}{suffix}({generic_t} '
                'x{args_def})'),
            'body': (
                'generic_ref_t x_ref = init_generic_ref(x);\n'
                'return generic_ref_get_{name}(x_ref{args});\n'),
            'required_param': [
                'check', 'get', 'default', 'init_default'],
        },
        'getref': {
            'is_valid': lambda x: (not x.get('container', None)),
            'docs': (
                '@brief Get a {docs_name} from a generic '
                'item reference\n'
                '@param[in] x Generic item reference to retrieve data from\n'
                '{args_docs}'),
            'definition': (
                '{ret_type} generic_ref_get_{name}{suffix}'
                '(generic_ref_t x{args_def})'),
            'body': (
                '{ret_type} out;\n'
                '{init_default}'
                'TRY_BEGIN'
                'if (!is_generic_ref_init(x)) {{\n'
                '  YggLogError << "{function}: Generic object is not '
                'initialized" '
                '<< std::endl;\n'
                '  return out;\n'
                '}}\n'
                'rapidjson::Value* d = (rapidjson::Value*)(x.obj);\n'
                'if (!{check}) {{\n'
                '  YggLogError << "{function}: Generic object is not '
                '{name}{message}: " << '
                '(*d) << std::endl;\n'
                '  return out;\n'
                '}}\n'
                '{get};\n'
                'TRY_END'
                'return out;\n'
            ),
            'required_param': ['check', 'get', 'default', 'init_default',
                               'message']
        },
        'get_cont': {
            'is_valid': lambda x: bool(x.get('container', None)),
            'docs': (
                '@brief Get a {docs_name} from an element in a {container}\n'
                '@param[in] x {container} to get element from\n'
                '{args_docs}'),
            'definition': (
                '{ret_type} generic_{container}_get_{name}{suffix}'
                '({generic_t} x{args_def})'),
            'body': (
                '{ret_type} out;\n'
                '{init_default}'
                'TRY_BEGIN'
                'generic_ref_t item = init_generic_ref(x);\n'
                'if (get_generic_{container}_ref(x, {idx}, &item) != '
                'GENERIC_SUCCESS_) {{\n'
                '  return out;\n'
                '}}\n'
                'out = generic_ref_get_{name}(item{args});\n'
                'TRY_END'
                'return out;\n'),
            'required_param': ['default', 'init_default']
        },
    }
    param = {
        'ret_type': {
            'size': {'default': 'size_t'},
            'set': {
                'default': 'int',
            },
            'get': {
                'default': '{type}',
                'scalar': '{scalar_type}',
                '1darray': 'size_t',
                'ndarray': 'size_t',
                'raw_scalar': 'void*',
                'raw_1darray': 'size_t',
                'raw_ndarray': 'size_t',
            },
        },
        'args_docs': {
            'set': {
                'default': '@param[in] value The value to assign to x\n',
                'scalar': ('@param[in] value The value to assign to x\n'
                           '@param[in] units Units of value\n'),
                '1darray': (
                    '@param[in] value The array of values to assign to x\n'
                    '@param[in] length The number of elements in value\n'
                    '@param[in] units Units of value\n'),
                'ndarray': (
                    '@param[in] value The array of values to assign to x'
                    '\n  in row-major order\n'
                    '@param[in] ndim The number of dimensions in value, '
                    'or 0 on error\n'
                    '@param[in] shape The size of value in each dimension\n'
                    '@param[in] units Units of value\n'),
                'raw_scalar': (
                    '@param[in] value Pointer to the memory containing '
                    'the value to assign to x\n'
                    '@param[in] subtype Subtype of data contained in value\n'
                    '@param[in] precision The precision of the data in value\n'
                    '@param[in] units Units of value\n'),
                'raw_1darray': (
                    '@param[in] value Pointer to the memory containing '
                    'the array to assign\n  to x\n'
                    '@param[in] subtype Subtype of data contained in value\n'
                    '@param[in] precision The precision of the elements '
                    'in value\n'
                    '@param[in] length The number of elements in value\n'
                    '@param[in] units Units of value\n'),
                'raw_ndarray': (
                    '@param[in] value Pointer to the memory containing '
                    'the array to assign\n  to x\n'
                    '@param[in] subtype Subtype of data contained in value\n'
                    '@param[in] precision The precision of the elements '
                    'in value\n'
                    '@param[in] ndim The number of dimensions in value\n'
                    '@param[in] shape The size of value in each dimension\n'
                    '@param[in] units Units of value\n'),
            },
            'get': {
                'default': (
                    '@returns Value from x\n'),
                '1darray': (
                    '@param[out] value Pointer to memory that should be'
                    ' reallocated and\n  filled with the array contents'
                    ' of x\n'
                    '@returns Number of elements in the array, '
                    'or 0 on error\n'),
                'ndarray': (
                    '@param[out] value Pointer to memory that should be'
                    ' reallocated and\n  filled with the array contents'
                    ' of x in row-major order\n'
                    '@param[out] shape Pointer to memory that should be'
                    ' reallocated and\n  filled with the size of the'
                    ' array in each dimension\n'
                    '@returns Number of dimensions in the array, '
                    'or 0 on error\n'),
                'raw_scalar': (
                    '@param[in] subtype Subtype of data to return\n'
                    '@param[in] precision Precision of the data to return\n'
                    '@returns Pointer to value in x\n'),
                'raw_1darray': (
                    '@param[in] subtype Subtype of data to return\n'
                    '@param[in] precision Precision of the data to return\n'
                    '@param[out] value Pointer to memory that should be'
                    ' reallocated and\n  filled with the array contents'
                    ' of x\n'
                    '@returns Number of elements in the array, '
                    'or 0 on error\n'),
                'raw_ndarray': (
                    '@param[in] subtype Subtype of data to return\n'
                    '@param[in] precision Precision of the data to return\n'
                    '@param[out] value Pointer to memory that should be'
                    ' reallocated and\n  filled with the array contents'
                    ' of x in row-major order\n'
                    '@param[out] shape Pointer to memory that should be'
                    ' reallocated and\n  filled with the size of the'
                    ' array in each dimension\n'
                    '@returns Number of dimensions in the array, '
                    'or 0 on error\n'),
                'item': (
                    '@param[in] type Type of item to retrieve\n'
                    '@returns Pointer to data containing raw item data,'
                    ' NULL on error\n'),
                'item_nbytes': (
                    '@param[in] type Type of item to retrieve\n'
                    '@returns Number of bytes in raw item data, 0 on error\n'),
            },
            'size': {
                'default': ''
            },
        },
        'args': {
            'size': {'default': ''},
            'set': {
                'default': ', value',
                'scalar': ', value, units',
                '1darray': ', value, length, units',
                'ndarray': ', value, ndim, shape, units',
                'raw_scalar': ', value, subtype, precision, units',
                'raw_1darray': (', value, subtype, precision, length, '
                                'units'),
                'raw_ndarray': (', value, subtype, precision, ndim, '
                                'shape, units'),
                'item': ', type, value',
                'item_nbytes': ', type, value',
            },
            'get': {
                'default': '',
                '1darray': ', value',
                'ndarray': ', value, shape',
                'raw_scalar': ', subtype, precision',
                'raw_1darray': ', subtype, precision, value',
                'raw_ndarray': ', subtype, precision, value, shape',
                'item': ', type',
                'item_nbytes': ', type',
            },
        },
        'args_def': {
            'size': {'default': ''},
            'set': {
                'default': ', const {type} value',
                'string': ', {type} value',
                'scalar': (
                    ', const {scalar_type} value, const char* units'),
                '1darray': (
                    ', const {scalar_type}* value, const size_t length'
                    ', const char* units'),
                'ndarray': (
                    ', const {scalar_type}* value, const size_t ndim, '
                    'const size_t* shape, const char* units'),
                'raw_scalar': (
                    ', const void* value, const char* subtype, '
                    'const size_t precision, '
                    'const char* units'),
                'raw_1darray': (
                    ', const void* value, const char* subtype, '
                    'const size_t precision, '
                    'const size_t length, '
                    'const char* units'),
                'raw_ndarray': (
                    ', const void* value, const char* subtype, '
                    'const size_t precision, '
                    'const size_t ndim, '
                    'const size_t* shape, '
                    'const char* units'),
                'item': ', const char* type, void* value',
                'item_nbytes': ', const char* type, void* value',
            },
            'get': {
                'default': '',
                '1darray': ', {scalar_type}** value',
                'ndarray': ', {scalar_type}** value, size_t** shape',
                'raw_scalar': ', const char* subtype, const size_t precision',
                'raw_1darray': (', const char* subtype, '
                                'const size_t precision, void** value'),
                'raw_ndarray': (', const char* subtype, '
                                'const size_t precision, void** value, '
                                'size_t** shape'),
                'item': ', const char* type',
                'item_nbytes': ', const char* type',
            },
        },
        'get_size': {
            'array': 'out = (size_t)(d->Size())',
            'object': 'out = (size_t)(d->MemberCount())',
        },
        'get': {
            'null': 'out = NULL',
            'boolean': 'out = d->GetBool()',
            'integer': 'out = d->GetInt()',
            'number': 'out = d->GetDouble()',
            'complex': (
                'std::complex<{X}> tmp = d->GetScalar<std::complex<{X}> >();\n'
                'out.re = tmp.real();\n'
                'out.im = tmp.imag()'),
            'scalar': 'out = d->GetScalar<{scalar_cpp_type}>()',
            '1darray': (
                'rapidjson::SizeType nelements = 0;\n'
                'value[0] = ({scalar_type}*)(d->Get1DArray<'
                '{scalar_cpp_type}>(nelements, '
                'generic_ref_allocator(x)));\n'
                'out = (size_t)nelements'),
            'ndarray': (
                'rapidjson::SizeType ndim = 0;\n'
                'rapidjson::SizeType* rjshape = NULL;\n'
                'value[0] = ({scalar_type}*)(d->GetNDArray<'
                '{scalar_cpp_type}>('
                'rjshape, ndim, generic_ref_allocator(x)));\n'
                'shape[0] = (size_t*)(generic_ref_allocator(x).'
                'Malloc(ndim * sizeof(size_t)));\n'
                'for (rapidjson::SizeType i = 0; i < ndim; i++) {{\n'
                '  (*shape)[i] = rjshape[i];\n'
                '}}\n'
                'generic_ref_allocator(x).Free(rjshape);\n'
                'out = (size_t)ndim'),
            'generic': (
                'rapidjson::Document* cpy = new rapidjson::Document();\n'
                'cpy->CopyFrom(*d, cpy->GetAllocator(), true);\n'
                'out.obj = (void*)cpy'),
            'python': 'out.obj = d->GetPythonObjectRaw()',
            'geometry': (
                '{cpp_type} tmp;\n'
                'd->Get{geom_name}(tmp);\n'
                'set_{name}(&out, (void*)(&tmp), 1)'),
            'raw_scalar': (
                'out = generic_ref_get_item(x, "scalar")'),
            'raw_1darray': (
                'void* new_data = generic_ref_get_item(x, "1darray");\n'
                'assert(new_data);\n'
                'size_t nbytes = generic_ref_get_item_nbytes(x, "1darray");\n'
                'out = (size_t)(d->GetNElements());\n'
                'value[0] = generic_ref_allocator(x).Realloc(value[0], '
                '0, nbytes);\n'
                'if (value[0] == NULL) {{\n'
                '  ygglog_throw_error("{function}: Failed to reallocate '
                'array");\n'
                '}}\n'
                'memcpy(value[0], new_data, nbytes)'),
            'raw_ndarray': (
                'void* new_data = generic_ref_get_item(x, "ndarray");\n'
                'assert(new_data);\n'
                'size_t nbytes = generic_ref_get_item_nbytes(x, "ndarray");\n'
                'value[0] = generic_ref_allocator(x).Realloc(value[0], '
                '0, nbytes);\n'
                'if (value[0] == NULL) {{\n'
                '  ygglog_throw_error("{function}: Failed to reallocate '
                'array");\n'
                '}}\n'
                'memcpy(value[0], new_data, nbytes);\n'
                'const rapidjson::Value& rjshape = d->GetShape();\n'
                'out = (size_t)(rjshape.Size());\n'
                'shape[0] = (size_t*)(generic_ref_allocator(x).Realloc('
                'shape[0], 0, out * sizeof(size_t)));\n'
                'if (shape[0] == NULL) {{\n'
                '  ygglog_throw_error("{function}: Failed to reallocate '
                'shape.");\n'
                '}}\n'
                'size_t i = 0;\n'
                'for (rapidjson::Value::ConstValueIterator it = '
                'rjshape.Begin();\n'
                '     it != rjshape.End(); it++, i++) {{\n'
                '  shape[0][i] = (size_t)(it->GetInt());\n'
                '}}'),
            'item': (
                'bool requires_freeing = false;\n'
                'out = d->GetDataPtr(requires_freeing)'),
            'item_nbytes': 'out = d->GetNBytes()',
        },
        'set': {
            'null': 'd->SetNull(); UNUSED(value)',
            'integer': 'd->SetInt(value)',
            'number': 'd->SetDouble(value)',
            'boolean': 'd->SetBool(value)',
            'string': (
                'd->SetString(value, STRLEN_RJ(value), '
                'generic_allocator(x))'),
            'scalar': 'd->SetScalar(value, units, generic_allocator(x))',
            '1darray': (
                'd->Set1DArray(({scalar_cpp_type}*)value, '
                '(rapidjson::SizeType)length, '
                'units, generic_allocator(x))'),
            'ndarray': (
                'rapidjson::SizeType* rjshape = (rapidjson::SizeType*)'
                '(generic_allocator(x).Malloc(ndim * '
                'sizeof(rapidjson::SizeType)));\n'
                'for (size_t i = 0; i < ndim; i++) {{\n'
                '  rjshape[i] = (rapidjson::SizeType)(shape[i]);'
                '}}\n'
                'd->SetNDArray(({scalar_cpp_type}*)value, '
                'rjshape, (rapidjson::SizeType)ndim, '
                'units, generic_allocator(x));\n'
                'generic_allocator(x).Free(rjshape);'),
            'complex': (
                'd->SetScalar({scalar_cpp_type}(value.re, value.im), '
                'units, generic_allocator(x))'),
            'generic': (
                'd->CopyFrom(*((rapidjson::Value*)(value.obj)), '
                'generic_allocator(x), true)'),
            'python': (
                'd->SetPythonObjectRaw(value.obj, '
                'generic_allocator(x))'),
            'geometry': (
                'd->Set{geom_name}(*(({cpp_type}*)(value.obj)), '
                'generic_allocator(x))'),
            'schema': (
                'd->SetSchema(*((rapidjson::Value*)(value.obj)), '
                'generic_allocator(x))'),
            'raw_varient': (
                'rapidjson::Document schema(rapidjson::kObjectType);\n'
                'schema.AddMember(rapidjson::Document::GetTypeString(),\n'
                '                 rapidjson::Value("{name}", {name_len},\n'
                '                    schema.GetAllocator()).Move(),\n'
                '                 schema.GetAllocator());\n'
                'schema.AddMember(rapidjson::Document::GetSubTypeString(),\n'
                '                 '
                'rapidjson::Value(subtype, STRLEN_RJ(subtype),\n'
                '                                  '
                'schema.GetAllocator()).Move(),\n'
                '                 schema.GetAllocator());\n'
                'schema.AddMember('
                'rapidjson::Document::GetPrecisionString(),\n'
                '                 '
                'rapidjson::Value((unsigned)precision).Move(),\n'
                '                 '
                'schema.GetAllocator());\n'
                'if (units && strlen(units) > 0) {{\n'
                '  schema.AddMember('
                'rapidjson::Document::GetUnitsString(),\n'
                '                   '
                'rapidjson::Value(units, STRLEN_RJ(units),\n'
                '                                    '
                'schema.GetAllocator()).Move(),\n'
                '                   schema.GetAllocator());\n'
                '}}\n'
                '{set_shape}'
                'd->SetYggdrasilString('
                '(char*)value, precision * length,\n'
                '                      generic_allocator(x),\n'
                '                      schema)'),
            'item': (
                'if (!d->SetDataPtr(type, value, '
                'generic_allocator(x))) {{\n'
                '  YggLogError << "{function}: Error setting data '
                'pointer" << std::endl;\n'
                '  return GENERIC_ERROR_;\n'
                '}}'),
        },
        'set_shape': {
            'default': '',
            'raw_scalar': 'size_t length = 1;\n',
            'raw_1darray': (
                'rapidjson::Value rjshape(rapidjson::kArrayType);\n'
                'rjshape.PushBack('
                'rapidjson::Value((unsigned)length).Move(),\n'
                '                 schema.GetAllocator());\n'
                'schema.AddMember('
                'rapidjson::Document::GetShapeString(), rjshape,\n'
                '                 schema.GetAllocator());\n'),
            'raw_ndarray': (
                'rapidjson::Value rjshape(rapidjson::kArrayType);\n'
                'size_t length = 1;\n'
                'if (ndim <= 0)\n'
                '  length = 0;\n'
                'for (size_t i = 0; i < ndim; i++) {{\n'
                '  rjshape.PushBack('
                'rapidjson::Value((unsigned)(shape[i])).Move(),\n'
                '                   schema.GetAllocator());\n'
                '  length *= shape[i];\n'
                '}}\n'
                'schema.AddMember('
                'rapidjson::Document::GetShapeString(), rjshape,\n'
                '                 schema.GetAllocator());\n'),
        },
        'message': {
            'default': '',
            'raw_scalar': (
                ' of subtype \'" << std::string(subtype) << "\' with '
                'precision " << precision << "'),
            'raw_1darray': (
                ' of subtype \'" << std::string(subtype) << "\' with '
                'precision " << precision << "'),
            'raw_ndarray': (
                ' of subtype \'" << std::string(subtype) << "\' with '
                'precision " << precision << "'),
        },
        'check': {
            'boolean': 'd->IsBool()',
            'integer': 'd->IsInt()',
            'scalar': 'd->IsScalar<{scalar_cpp_type}>()',
            '1darray': 'd->Is1DArray<{scalar_cpp_type}>()',
            'ndarray': 'd->IsNDArray<{scalar_cpp_type}>()',
            'obj': 'd->IsObjWavefront()',
            'class': 'd->IsPythonClass()',
            'instance': 'd->IsPythonInstance()',
            'function': 'd->IsPythonFunction()',
            'any': 'true',
            'schema': 'd->IsSchema()',
            'scalar': 'd->IsScalar<{scalar_cpp_type}>()',
            '1darray': 'd->Is1DArray<{scalar_cpp_type}>()',
            'ndarray': 'd->IsNDArray<{scalar_cpp_type}>()',
            'raw_scalar': (
                '(d->IsType("{name}") && d->IsSubType(subtype, '
                'static_cast<rapidjson::SizeType>(precision)))'),
            'raw_1darray': (
                '(d->IsType("{name}") && d->IsSubType(subtype, '
                'static_cast<rapidjson::SizeType>(precision)))'),
            'raw_ndarray': (
                '(d->IsType("{name}") && d->IsSubType(subtype, '
                'static_cast<rapidjson::SizeType>(precision)))'),
            'item': 'd->IsType(type)',
            'item_nbytes': 'd->IsType(type)',
        },
        'default': {
            'default': '0',
            'null': 'NULL',
            'boolean': 'false',
            'integer': '0',
            'number': '0.0',
            'string': '""',
            'int': '0',
            'uint': '0',
            'float': '0.0',
            'bytes': '""',
            'unicode': '""',
            'generic': 'init_generic()',
            'array': 'init_generic()',
            'object': 'init_generic()',
            'python': 'init_python()',
            'geometry': 'init_{name}()',
            'raw_scalar': 'NULL',
            'item': 'NULL',
            'item_nbytes': '-1',
        },
        'init_default': {
            'default': 'out = {default};\n',
            'complex': 'out.re = 0.0;\nout.im = 0.0;\n',
        },
    }
    file_suffix = (
        '\n\n#undef GENERIC_SUCCESS_\n#undef GENERIC_ERROR_'
        '\n\n} // extern C\n')

    def __init__(self, src=None, header=None, fortran=None,
                 also_wrap=None):
        if src is None:
            src = os.path.join('cpp', 'src', 'datatypes', 'dtype_t.cpp')
        if header is None:
            header = os.path.join('cpp', 'include', 'datatypes',
                                  'dtype_t.h')
        if also_wrap is None:
            also_wrap = [os.path.join('cpp', 'include', 'YggInterface.h'),
                         os.path.join('cpp', 'include', 'communicators',
                                      'comm_t.hpp')]
        super(CFile, self).__init__('c', src)
        if header:
            self.added['header'] = CFile(header, header=False)
            self.added['header'].file_suffix = (
                '\n\n#ifdef __cplusplus\n}\n#endif\n')
            # '\n\n#endif /*init once*/\n')
        if fortran:
            self.added['fortran'] = fortran
        if also_wrap:
            self.added['also_wrap'] = also_wrap

    def init_param(self):
        super(CFile, self).init_param()
        self.types['inv_all']['generic_t'] = 'any'
        for k in ['python', 'geometry', 'generic', 'varient']:
            for t in self.types[k]:
                for x in self.param.keys():
                    if k in self.param[x]:
                        self.param[x].setdefault(t, self.param[x][k])
        for t in self.types['container']:
            for x in self.param.keys():
                if 'generic' in self.param[x]:
                    self.param[x].setdefault(t, self.param[x]['generic'])
        for t in self.types['varient']:
            for x in self.param.keys():
                if 'raw_varient' in self.param[x]:
                    self.param[x].setdefault(
                        f'raw_{t}', self.param[x]['raw_varient'])
        for k, v in self.types['all'].items():
            if k not in self.cpp_types:
                self.cpp_types[k] = v
            if k not in self.types['scalar']:
                if k not in self.param['check']:
                    self.param['check'][k] = f'd->Is{k.title()}()'
                if k not in self.types['container']:
                    if k not in self.param['set']:
                        self.param['set'][k] = (
                            f'd->Set{k.title()}(value)')
                    if k not in self.param['get']:
                        self.param['get'][k] = (
                            f'out = d->Get{k.title()}()')
        for x in ['set', 'get']:
            self.param['args'][f'{x}_cont'] = self.param['args'][x]
            self.param['args_def'][f'{x}_cont'] = {
                k: ', {idx_type} {idx}' + v
                for k, v in self.param['args_def'][x].items()}
            self.param['args_docs'][f'{x}_cont'] = {
                k: ('@param[in] {idx} {idx} of element to '
                    + x + '\n') + v
                for k, v in self.param['args_docs'][x].items()}
            self.param['ret_type'][f'{x}_cont'] = self.param['ret_type'][x]
        for k in ['args', 'args_def', 'args_docs', 'ret_type']:
            self.param[k]['getref'] = self.param[k]['get']
        self.regexes = copy.deepcopy(self.regexes)
        self.format_dict(self.regexes)

    def generate(self, **kwargs):
        if 'fortran' in self.added:
            if 'also_wrap' in self.added:
                for x in self.added['also_wrap']:
                    with open(os.path.join(_base_dir, x), 'r') as fd:
                        contents = fd.read()
                    assert contents
                    self.wrapfortran(contents)
            self.wrapfortran(self.added['header'].contents)
        return super(CFile, self).generate(**kwargs)

    def wrapfortran(self, contents, **kwargs):
        one_match = False
        kwargs.setdefault('basefile', self)
        for match in re.finditer(self.regexes['declare_func'],
                                 contents,
                                 flags=re.MULTILINE):
            one_match = True
            kws = dict(match.groupdict(), **kwargs)
            self.complete_fortran_args(kws)
            assert kws['basefile']
            self.added['fortran'].add_method(kws)
        assert one_match

    def add_method(self, kwargs, **kws):
        kwargs.setdefault('suffix', '')
        if 'xdocs' not in kwargs:
            xdocs = self.methods[kwargs['action']].get('docs', '')
            if kwargs['action'] in ['get', 'getref']:
                if kwargs['raw_type'] == 'item':
                    xdocs = (
                        '@brief Get the raw item data\n'
                        '@param[in] x Generic item to retrieve data from\n'
                        '{args_docs}')
                elif kwargs['raw_type'] == 'item_nbytes':
                    xdocs = (
                        '@brief Get the size of the raw item data\n'
                        '@param[in] x Generic item to retrieve data size '
                        'from\n{args_docs}')
            kwargs['xdocs'] = xdocs
        kwargs.setdefault(
            'xdef', self.methods[kwargs['action']]['definition'])
        kwargs.setdefault(
            'xbody', self.methods[kwargs['action']]['body'])
        kwargs.setdefault(
            'xfunc',
            kwargs['xdef'].split('{suffix}(', 1)[0].rsplit(' ', 1)[-1])
        kwargs.setdefault('function', kwargs['xfunc'])
        return super(CFile, self).add_method(kwargs, **kws)

    def format(self, kwargs):
        kwargs['return'] = (
            '' if kwargs.get('ret_type', None) == 'void' else 'return ')
        kwargs.setdefault('body_prefix', '')
        kwargs.setdefault('body_suffix', '')
        function = kwargs['function']
        suffix = kwargs['suffix']
        try:
            definition = kwargs['xdef'].format(**kwargs)
            body = kwargs['xbody'].format(**kwargs)
        except KeyError:
            print(kwargs['xdef'])
            print(kwargs['xbody'])
            pprint.pprint(kwargs)
            raise
        if self.try_begin in body:
            assert self.try_end in body
            before, contents = body.split(self.try_begin)
            contents, after = contents.split(self.try_end)
            contents = contents.replace('\n', '\n' + self.indent)
            contents = contents.rstrip()
            body = (
                f'{before}try {{\n{self.indent}'
                f'{contents}\n'
                f'}} catch(...) {{\n'
                f'  YggLogError << "{function}: C++ exception thrown" '
                f'<< std::endl;\n'
                f'}}\n'
                f'{after}')
        body = self.indent + body.replace('\n', '\n' + self.indent)
        body = body.rstrip()
        docs = self.format_docs(kwargs)
        api = kwargs.get('api', 'YGG_API ')
        header_definition = f'{docs}{api}{definition};'
        self.added['header'].append(header_definition)
        for k in ['name', 'container']:
            if kwargs.get(k, '') in self.container_aliases:
                ikw = dict(kwargs)
                ikw[k] = self.container_aliases[kwargs[k]]
                aliased_function = kwargs['xfunc'].format(**ikw)
                self.added['header'].append(
                    f'#define {aliased_function}{suffix} '
                    f'{function}{suffix}')
        if 'fortran' in self.added:
            kws = dict(
                kwargs,
                **re.search(self.regexes['declare_func'],
                            header_definition,
                            flags=re.MULTILINE).groupdict())
            for k in ['xbody', 'xdef', 'args']:
                kws.pop(k)
            assert 'xbody' not in kws
            self.complete_fortran_args(kws)
            self.added['fortran'].add_method(kws)
            stop_on = 'destroy_generic'
            if stop_on and stop_on in kwargs['function']:
                pprint.pprint(kwargs)
                raise Exception
        return f'{definition} {{\n{body}\n}}'

    def generate_long_double(self, **kwargs):
        prefix = '#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE'
        suffix = '#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE'
        self.lines.append(prefix)
        self.added['header'].append(prefix)
        if 'fortran' in self.added:
            self.added['fortran'].lines.append(prefix)
            self.added['fortran'].added['cdefs'].lines.append(prefix)
            self.added['fortran'].added['cwrap'].lines.append(prefix)
            self.added['fortran'].added['cwrap'].added[
                'header'].lines.append(prefix)
        super(CFile, self).generate_long_double(**kwargs)
        self.lines.append(suffix)
        self.added['header'].append(suffix)
        if 'fortran' in self.added:
            self.added['fortran'].lines.append(suffix)
            self.added['fortran'].added['cdefs'].lines.append(suffix)
            self.added['fortran'].added['cwrap'].lines.append(suffix)
            self.added['fortran'].added['cwrap'].added[
                'header'].lines.append(suffix)

    def _set_raw_type(self, x, kwargs, try_types=None,
                      type_class='all'):
        if ((x['ptr'] and x['base'] == 'void'
             and ((x.get('is_return')
                   and kwargs['function'].endswith(('get_item',
                                                    'get_scalar',
                                                    'get_1darray',
                                                    'get_ndarray')))
                  or kwargs['function'].endswith(('set_item',
                                                  'set_scalar',
                                                  'set_1darray',
                                                  'set_ndarray'))))):
            x['raw_type'] = 'pointer'
            return
        if try_types is None:
            try_types = [x['type'], x['base'] + x['ptr']]
        try_replace = ['X', 'X_name', 'X_bytes']
        for k in try_replace:
            if kwargs.get(k, None):
                for k_rep in try_replace:
                    try_types += [
                        x.replace(kwargs[k], f'{{{k_rep}}}')
                        for x in try_types if kwargs[k] in x]
        for t in try_types:
            if t in self.types[f'inv_{type_class}']:
                x['raw_type'] = self.types[f'inv_{type_class}'][t]
                if x['raw_type'] in self.types['scalar']:
                    x['scalar_raw_type'] = x['raw_type']
                    x['scalar_type'] = x['base']
                    if x.get('ptr', None):
                        # TODO: differentiate with 1d & nd arrays
                        x['raw_type'] = 'ndarray'
                    else:
                        x['raw_type'] = 'scalar'
                break
        if (('raw_type' in x and x.get('is_return', False)
             and x['raw_type'] in ['string'])):
            x['raw_type'] = 'string_return'

    def complete_fortran_arg(self, x, kwargs):
        for k in ['X', 'X_name', 'X_bytes']:
            if kwargs.get(k, None):
                x[k] = kwargs[k]
        x['type'] = self.typedef.format(**x)
        self._set_raw_type(x, kwargs)
        if 'raw_type' not in x:
            if x['ptr']:
                self._set_raw_type(x, kwargs, try_types=[x['base']],
                                   type_class='scalar')
                if 'raw_type' not in x:
                    self._set_raw_type(x, kwargs, try_types=[x['base']])
                    if 'raw_type' in x and x['raw_type'] != 'noret':
                        x['base_raw_type'] = x['raw_type']
                    x['raw_type'] = 'pointer'
            elif 'enum' in x['type']:
                x['raw_type'] = 'integer'
        if 'raw_type' not in x:
            pprint.pprint(kwargs)
            pprint.pprint(x)
        assert 'raw_type' in x

    def complete_fortran_args(self, kwargs):
        if 'raw_args' in kwargs:
            return
        if kwargs.get('docs', None):
            kwargs['docs'] = '\n'.join(
                x.lstrip(self.docs_c) for x in
                kwargs['docs'].splitlines())
        if ((kwargs['args_def'].startswith('(')
             and kwargs['args_def'].endswith(')'))):
            kwargs['args_def'] = kwargs['args_def'].split(
                '(', 1)[-1].rsplit(')', 1)[0]
        kwargs['action'] = ''
        kwargs['suffix'] = '_f'
        add_args = [('out', 'ret_type')]
        kwargs['raw_args'] = [
            match.groupdict() for match in
            re.finditer(self.regexes['declare_args'],
                        kwargs['args_def'])]
        kwargs['raw_args'] += [
            dict(re.search(self.regexes['type'], kwargs[k]).groupdict(),
                 name=n, type=kwargs[k], key=k,
                 is_return=(k == 'ret_type'))
            for n, k in add_args if k in kwargs]
        if 'args' not in kwargs:
            kwargs['args'] = ', '.join(
                x['name'] for x in kwargs['raw_args']
                if not x.get('is_return', False))
        for x in kwargs['raw_args']:
            for k in ['const', 'ptr', 'shape']:
                if x.get(k, None) is None:
                    x[k] = ''
            if 'base' not in x and 'type' in x:
                x['base'] = x['type']
            new_type = self.typedef.format(**x)
            x['type'] = new_type
            if not x.get('length', None):
                x['length'] = '*'
            # TODO: Transform shape
            self.complete_fortran_arg(x, kwargs)
            x['c_type'] = copy.deepcopy(x)
            if (('*' in x['type'] and ((x['type'] in self.char_types
                                        and x.get('is_return', False))
                                       or x['type'] not in self.char_types)
                 or x['type'] == 'c_function')):
                x['base'] = 'void'
                x['ptr'] = '*'
                self.complete_fortran_arg(x, kwargs)
        # TODO: cast c_function
        args = []
        args_def = []
        for x in kwargs['raw_args']:
            if x.get('is_return', False):
                continue
            if x['c_type']['raw_type'] == 'registered_function':
                kwargs.setdefault('body_prefix', '')
                kwargs['body_prefix'] += (
                    f"bool ({x['name']}_int*)(generic_t, generic_t) = "
                    f"{x['name']};\n")
                args.append(f"*{x['name']}_int")
            elif x['c_type']['type'] != x['type']:
                args.append(
                    self.castdef.format(
                        **dict(x, c_type=x['c_type']['type'])))
            else:
                args.append(x['name'])
            args_def.append(self.argsdef.format(**x))
        kwargs['args'] = ', '.join(args)
        kwargs['args_def'] = ', '.join(args_def)
        for k in ['docs']:
            if kwargs.get(k, '') is None:
                kwargs[k] = ''


class FortranWrapperFile(CFile):

    def __init__(self, src=None, header=None):
        if src is None:
            src = os.path.join('fortran', 'c_wrappers.c')
        if header is None:
            header = os.path.join('fortran', 'c_wrappers.h')
        super(FortranWrapperFile, self).__init__(src=src, header=header)
        self.file_suffix = ''

    def add_method(self, kwargs, **kws):
        kwargs.setdefault('action', '')
        return super(FortranWrapperFile, self).add_method(kwargs, **kws)


class FortranFile(AmendedFile):

    comment = '!'
    docs_c = '!> '
    begin_doc_block = ''
    end_doc_block = ''
    indent = '  '
    indent_append = '  '
    file_suffix = '\n\nend module YggInterface\n'
    internal_types = [
        'yggnull', 'ygggeneric', 'yggply', 'yggobj', 'yggschema',
        'yggpython', 'yggpyfunc', 'yggpyinst', 'yggcomm', 'yggdtype',
        'ygggenericref', 'yggchar_r', 'yggcomplex_float',
        'yggcomplex_double', 'yggcomplex_long_double',
    ]
    iso_types = {'void*': 'c_ptr', 'bool': 'c_bool',
                 'c_function': 'c_funptr',
                 'long double': 'c_long_double',
                 'complex_float_t': 'c_float_complex',
                 'complex_double_t': 'c_double_complex',
                 'complex_long_double_t': 'c_long_double_complex'}
    aliased_functions = {
        'destroy_generic': 'free_generic',
        'destroy_python': 'free_python',
        'global_scope_comm_on_c': 'set_global_comm',
        'global_scope_comm_off_c': 'unset_global_comm',
    }
    excluded_functions = ['ygg_rpc_client_type', 'ygg_rpc_server_type',
                          'ygg_send', 'ygg_recv', 'comm_recv',
                          'is_comm_format_array_type',
                          'set_obj', 'set_ply']
    checked_functions = {
        'init_python_api': {
            'cond': '{c_ret_name}.lt.0',
            'message': 'Error initializing Python.',
        },
        ('generic_object_set_', 'generic_array_set_'): {
            'cond': '{c_ret_name}.lt.0',
            'message': 'Error setting element',
        },
        ('generic_array_get_item_nbytes',
         'generic_object_get_item_nbytes',
         'generic_get_item_nbytes'): {
             'cond': '{c_ret_name}.lt.0',
             'message': 'Error getting element',
             'return': True,
         },
        ('is_dtype_format_array', ): {
            'cond': '({c_ret_name}.ne.0).and.({c_ret_name}.ne.1)',
            'message': 'Error checking data type',
            'return': True,
        },
        ('copy_generic_into', ): {
            'cond': '{c_ret_name}.ne.0',
            'message': 'Error copying generic object.',
        },
        ('free_generic', ): {
            'cond': '{c_ret_name}.ne.0',
            'message': 'Error freeing generic object.',
        },
    }
    optional_arguments = {
        'generic_object_set_scalar': ['units'],
        'generic_object_set_1darray': ['units'],
        'generic_object_set_ndarray': ['units'],
        'generic_array_set_scalar': ['units'],
        'generic_array_set_1darray': ['units'],
        'generic_array_set_ndarray': ['units'],
        'ygg_rpc_client': ['outFormat', 'inFormat'],
        'ygg_rpc_server': ['outFormat', 'inFormat'],
    }
    bypassed_arguments = {
        'generic_object_set_ndarray': {
            'ndim': 'size(shape)',
        },
        'generic_array_set_ndarray': {
            'ndim': 'size(shape)',
        },
    }
    modified_arguments = {
        ('generic_array_get_', 'generic_array_set_'): {
            'index': '{b_name} = {a_name} - 1'
        },
        ('set_generic_array', 'get_generic_array'): {
            'i': '{b_name} = {a_name} - 1'
        },
        ('ygg_output_fmt', 'ygg_input_fmt'): {
            'fmtString': '{b_name} = convert_format_f2c({a_name})'
        },
        ('create_dtype_format', 'ygg_ascii_table_output',
         'ygg_ascii_array_output'): {
            'format_str': '{b_name} = convert_format_f2c({a_name})'
        },
        ('ygg_rpc_client', 'ygg_rpc_server'): {
            'outFormat': '{b_name} = convert_format_f2c({a_name})',
            'inFormat': '{b_name} = convert_format_f2c({a_name})',
        },
        ('ygg_init', ): {
            'out': {
                'after': '{a_name} = ({b_name}.eq.0)',
                'a_update': {'raw_type': 'boolean'},
            },
        },
        ('is_dtype_format_array', 'is_empty_dtype'): {
            'out': {
                'after': '{a_name} = ({b_name}.eq.1)',
                'a_update': {'raw_type': 'boolean'},
            },
        },
        ('ygg_send', ): {
            'out': {
                'after': '{a_name} = ({b_name}.ge.0)',
                'a_update': {'raw_type': 'boolean'},
            },
        },
        ('is_generic_init', ): {
            'out': {
                'after': '{a_name} = ({b_name}.ne.0)',
                'a_update': {'raw_type': 'boolean'},
            },
        },
        ('create_dtype_json_array', ): {
            'items': {
                'a_update': {'mods': ['dimension(:)', 'intent(in)']},
            },
        },
        ('create_dtype_json_object', ): {
            'values': {
                'a_update': {'mods': ['dimension(:)', 'intent(in)']},
            },
        },
    }
    conversions = {
        'in': {
            'default': {
                'before': "{b_name} = {a_name}"},
            'null': {
                'before': "{b_name} = {a_name}%ptr",
            },
            'string': {
                'before': "{b_name} = convert_string_f2c({a_name})",
                'after': "deallocate({b_name})",
                'b_update': {'mods': ['allocatable'],
                             'length': ':'},
            },
            'complex': {
                'before': (
                    "{b_name}%re = real({a_name})\n"
                    "{b_name}%im = aimag({a_name})"),
            },
            'shape': {
                'a_update': {'mods': ['target']},
                'before': "{b_name} = c_loc({a_name}(1))"},
            'string_array': {
                'a_update': {'mods': ['intent(in)', 'target']},
                'args_def': (
                    'character(kind=c_char, len=len({a_name}(1))),'
                    ' pointer :: {a_name}_iele\n'
                    'integer :: i, {a_name}_ilen\n'
                    'type(c_ptr), target :: {b_name}_int(size({a_name}))'),
                'before': (
                    'do i = 1, size({a_name})\n'
                    '   {a_name}_iele => {a_name}(i)\n'
                    '   {a_name}_ilen = len_trim({a_name}_iele)\n'
                    '   if ({a_name}_ilen.lt.len({a_name}_iele)) then\n'
                    '      {a_name}_iele(({a_name}_ilen+1):'
                    '({a_name}_ilen+1)) = c_null_char\n'
                    '   end if\n'
                    '   {b_name}_int(i) = c_loc({a_name}_iele(1:1))\n'
                    'end do\n'
                    '{b_name} = c_loc({b_name}_int(1))')},
            'dtype': {
                'before': "{b_name} = c_loc({a_name})",
                'a_update': {'mods': ['target']},
            },
            'ndarray': {
                'args_def': (
                    "type(yggptr) :: {b_name}_int"),
                'before': (
                    "{b_name}_int = yggarg({a_name})\n"
                    "{b_name} = {b_name}_int%ptr"),
            },
        },
        'out': {
            'default': {'after': "{a_name} = {b_name}"},
            'flag': {'after': "{a_name} = ({b_name}.ge.0)"},
            'flexible_string': {
                'b_update': {'length': 'len({a_name})+1'},
                'before': "{b_name} = {a_name}//c_null_char",
                'after': "{a_name} = {b_name}(:{b_length})"},
            'string': {
                'a_update': {'mods': ['pointer', 'intent(out)'],
                             'length': ':'},
                'b_update': {'length': ':'},
                'args_def': (
                    "integer :: i"),
                'after': (
                    "allocate(character(len=size({b_name})) :: {a_name})\n"
                    "do i = 1, size(x)\n"
                    "   {a_name}(i:i) = {b_name}(i)\n"
                    "enddo"),
            },
            'complex': {
                'after': (
                    "{a_name} = cmplx({b_name}%re, {b_name}%im)"),
            },
            'null': {
                'a_update': {'mods': ['pointer']},
                'after': "call c_f_pointer({b_name}, {a_name})"},
            'ndarray': {
                'c2f': True},
            'c2f': {
                'args_def': (
                    "type(yggptr) :: {b_name}_int\n"
                    "logical :: {b_name}_flag"),
                'before': (
                    "{b_name} = c_null_ptr\n"
                    "{b_name}_int = yggarg({a_name})"),
                'after': (
                    "{b_name}_int%ptr = {b_name}\n"
                    "{b_name}_flag = yggptr_c2f({b_name}_int, {realloc})\n"
                    "if (.not.{b_name}_flag) then\n"
                    "   stop \"Error recovering fortran pointer for "
                    "variable\"\n"
                    "end if"),
            },
            'shape_realloc': {
                'a_update': {'mods': ['pointer']},
                'args_def': (
                    'type(c_ptr), target :: {b_name}_target'),
                'before': (
                    '{b_name}_target = c_null_ptr\n'
                    '{b_name} = c_loc({b_name}_target)'),
                'after': (
                    'call c_f_pointer({b_name}_target, {a_name}, [c_out])'),
            },
            'string_return': {
                'a_update': {'length': ':', 'mods': ['allocatable']},
                'after': (
                    "{a_name} = convert_string_c2f({b_name})"),
            },
        },
    }
    types = {
        'core': {
            'null': 'yggnull',
            'boolean': 'logical',
            'integer': 'integer',
            'number': 'real(kind = 8)',
            'string': 'character(len = {length})',
        },
        'scalar': {
            'int': 'integer(kind = {X_bytes})',
            'float': 'real(kind = {X_bytes})',
            'complex': 'complex(kind = {X_bytes})',
            # 'bytes': 'character(len = {length})',
            # 'unicode': (
            #     'character(kind = selected_char_kind(\'ISO_10646\'), '
            #     'len = {length})'),
        },
        'container': {
            'array': 'ygggeneric',
            'object': 'ygggeneric',
        },
        'geometry': {
            'ply': 'yggply',
            'obj': 'yggobj',
        },
        'python': {
            'class': 'yggpython',
            'function': 'yggpyfunc',
            'instance': 'yggpyinst',
        },
        'varient': {
            'scalar': '{scalar_type}',
            '1darray': '{scalar_type}, dimension(:)',
            'ndarray': 'type({scalar_base}{X_bytes}_nd)',
        },
        'generic': {
            'schema': 'yggschema',
            'any': 'ygggeneric',
        },
        'util': {
            'comm': 'yggcomm',
            'dtype': 'yggdtype',
            'length': 'integer',
            'shape': 'integer(kind=c_size_t), dimension(:)',
            'shape_realloc': 'integer(kind=c_size_t), dimension(:)',
            'flag': 'logical',
            'generic_ref': 'ygggenericref',
            'noret': 'void',
            'flexible_string': 'character(len = {length})',
            'string_return': 'character(len={length})',
            'string_realloc': 'yggchar_r',
            'string_array': 'character(len={length}), dimension(:)',
            'registered_function': 'type(c_funptr)',
            'commflag': 'integer(kind=int64)',
            'pointer': 'type(c_ptr)',
            'timeout': 'integer(kind=int64)',
        },
    }
    scalar_varients = {
        'int': ['2', '4', '8'],
        'float': ['4', '8'],
        'complex': ['4', '8'],
    }
    typedef = '{type}{mods}'
    argsdef = (
        '{type}{mods} :: {name}{shape}'
    )
    optdef = (
        'if (present({name})) then\n'
        '   {present}\n'
        'else\n'
        '   {absent}\n'
        'end if'
    )
    isodef = '  use, intrinsic :: iso_c_binding, only: {iso_types}\n'
    importdef = '  import :: {internal_types}\n'
    wrapdef = (
        '{procedure} {function}({args}){result}\n'
        '  implicit none\n'
        '{args_def}'
        '{args_conv_in}'
        '  {c_result}{function}_c({c_args})\n'
        '{args_conv_out}'
        'end {procedure} {function}'
    )
    interface_excluded = (
        '_get_size',
        '_get_item',
        '_get_item_nbytes',
    )
    interfacedef_member = 'module procedure {name}'
    interfacedef = (
        'interface {name}\n'
        '   {members}\n'
        'end interface {name}')
    interface_docs = {
        'generic_array_get': (
            '!> @brief Get an element from an array.\n'
            '!> @param[in] x Generic object that is presumed to contain '
            'an array.\n'
            '!> @param[in] index Index of element to return.\n'
            '!> @param[out] out Variable to store element in. For ND '
            'arrays\n'
            '!>   this should be a reallocatable type of the form '
            '\\<type\\>\\<precision\\>_nd\n'
            '!>   (e.g. integer2_nd, real4_nd)\n'
        ),
        'generic_object_get': (
            '!> @brief Get an item from a map.\n'
            '!> @param[in] x Generic object that is presumed to contain '
            'a map.\n'
            '!> @param[in] key Key for item in the map that should be '
            'returned.\n'
            '!> @param[in] out Variable to store the item in.\n'
        ),
        'generic_array_set': (
            '!> @brief Set an element in an array.\n'
            '!> @param[in] x Generic object that is presumed to contain '
            'an array.\n'
            '!> @param[in] index Index for element that should be set.\n'
            '!> @param[in] val Variable containing value for the element.\n'
            '!> @param[in] units Optional units for scalars, 1D, & ND '
            'arrays.\n'
        ),
        'generic_object_set': (
            '!> @brief Set an item in a map.\n'
            '!> @param[in] x Generic object that is presumed to contain '
            'a map.\n'
            '!> @param[in] key Key string for item that should be set.\n'
            '!> @param[in] val Value to assign to the item.\n'
            '!> @param[in] units Units to assign to the item for '
            'scalars, 1D, & ND\n'
            '!>   arrays.\n'
        ),
    }

    def __init__(self, src=None, cdefs=None, cwrap=None,
                 cwrap_header=None, finterface=None, **kwargs):
        if src is None:
            src = os.path.join('fortran', 'YggInterface.F90')
        if cdefs is None:
            cdefs = os.path.join('fortran', 'YggInterface_cdef.F90')
        if cwrap is None:
            cwrap = os.path.join('fortran', 'c_wrappers.c')
        if finterface is None:
            finterface = os.path.join('fortran', 'YggInterface_interfaces.F90')
        if cdefs:
            cdefs = FortranCdefsFile(cdefs)
        if cwrap:
            cwrap = FortranWrapperFile(cwrap, cwrap_header)
        if finterface:
            finterface = FortranFile(
                finterface, cdefs=False, cwrap=False, cwrap_header=False,
                finterface=False,
                prefix_lines=['#ifndef DOXYGEN_SHOULD_SKIP_THIS'],
                suffix_lines=['#endif'])
        self.interfaces = {k: [] for k in self.interface_docs.keys()}
        super(FortranFile, self).__init__('fortran', src, **kwargs)
        if cdefs:
            self.added['cdefs'] = cdefs
        if cwrap:
            self.added['cwrap'] = cwrap
        if finterface:
            self.added['finterface'] = finterface

    def init_param(self):
        super(FortranFile, self).init_param()
        for k in ['int', 'size_t', 'float', 'double', 'char', 'long']:
            self.iso_types[k] = f'c_{k}'
            if k in CFile.scalar_varients:
                for v in CFile.scalar_varients[k]:
                    c_k = CFile.types['scalar'][k].format(X=v)
                    self.iso_types[c_k] = f'c_{c_k}'
        for k in ['null', 'boolean', 'integer', 'number',
                  'length', 'scalar', 'commflag', 'timeout']:
            self.conversions['in'].setdefault(
                k, self.conversions['in']['default'])
            self.conversions['out'].setdefault(
                k, self.conversions['out']['default'])

    def iso_container(self, x):
        if x in ['int', 'long', 'size_t'] or x.startswith('int'):
            fmt = 'integer(kind = c_{X})'
        elif x in ['float', 'double']:
            fmt = 'real(kind = c_{X})'
        elif x in ['bool']:
            fmt = 'logical(kind = c_{X})'
        elif x in ['char*']:
            fmt = self.types['core']['string']
        else:
            raise NotImplementedError(x)
        return fmt.format(X=x)

    def write(self, *args, **kwargs):
        if 'finterface' in self.added:
            for k, v in self.interfaces.items():
                if v:
                    members = [self.interfacedef_member.format(name=vv)
                               for vv in v]
                    members = '\n   '.join(members)
                    interface = (
                        self.interface_docs[k]
                        + self.interfacedef.format(name=k, members=members))
                    self.added['finterface'].append(
                        '\n' + self.indent_append
                        + interface.replace('\n', '\n' + self.indent_append))
        return super(FortranFile, self).write(*args, **kwargs)

    def format(self, kwargs):
        if ((kwargs.get('scalar_raw_type', None) in ['uint']
             or (kwargs.get('scalar_raw_type', None) == 'int'
                 and kwargs['X_bytes'] == '1'))):
            return ''
        docs = self.format_docs(kwargs)
        kwargs['function'] = camel2underscored(kwargs['function'])
        kwargs['check_function'] = False
        for k, v in self.checked_functions.items():
            if kwargs['function'].startswith(k):
                kwargs['check_function'] = v
                break
        if not kwargs['function'].endswith(self.interface_excluded):
            for k, v in self.interfaces.items():
                if kwargs['function'].startswith(k):
                    v.append(kwargs['function'])
        kwargs_cdefs = {}
        if 'cwrap' in self.added:
            kwargs_cwrap = copy.deepcopy(kwargs)
            kwargs_cwrap['api'] = ''
            self.added['cwrap'].add_method(kwargs_cwrap)
        if 'cdefs' in self.added:
            kwargs_cdefs = copy.deepcopy(kwargs)
            self.added['cdefs'].add_method(kwargs_cdefs)
        if kwargs['function'] in self.excluded_functions:
            return ''
        kwargs.update(
            generic_t=self.types['generic']['any'],
        )
        assert 'raw_args' in kwargs
        kwargs.pop('args')
        kwargs.pop('args_def')
        kwargs['raw_args'] = [x['c_type'] for x in kwargs['raw_args']]
        args_keys = [
            'iso_types', 'internal_types', 'args_def', 'args',
            'c_args', 'c_args_def', 'args_conv_in', 'args_conv_out']
        for k in args_keys:
            kwargs.setdefault(k, [])
            if isinstance(kwargs[k], str):
                kwargs[k] = kwargs[k].split(', ')
            if not isinstance(kwargs[k], list):
                assert isinstance(kwargs[k], list)
        for iout in kwargs['raw_args']:
            iout['is_arg'] = True
            self.parse_argument(iout, kwargs)
        if kwargs_cdefs:
            assert len(kwargs['raw_args']) == len(kwargs_cdefs['raw_args'])
            for a, b in zip(kwargs['raw_args'], kwargs_cdefs['raw_args']):
                b['is_c_arg'] = True
                b['mods'] = []
                assert a['name'] == b['name']
                assert a['raw_type'] == b['raw_type']
                if ((a['type'] != b['type']
                     or a['raw_type'] in ['string', 'flexible_string']
                     or 'base_raw_type' in a)):
                    b['name'] = f"c_{b['name']}"
                    self.add_conversion(a, b, kwargs)
                    kwargs['c_args_def'].append(b)
                else:
                    assert not (a['is_optional'] or a['is_modified']
                                or a['is_bypassed'])
                if a['is_return']:
                    kwargs['c_ret_name'] = b['name']
                    if a['type'] != 'void':
                        kwargs['c_result'] = b['name'] + ' = '
                else:
                    kwargs['c_args'].append(b['name'])
            kwargs['args_def'] += kwargs.pop('c_args_def')
        if kwargs['check_function']:
            check = kwargs['check_function']
            kwargs['args_conv_out'] += (
                f"if ({check['cond']}) then\n"
                f"   stop \"{kwargs['function']}: {check['message']}\"\n"
                f"end if").format(**kwargs).splitlines()
        kwargs['args_def'] = self.format_argsdef(kwargs['args_def'])
        for k in args_keys:
            if k in ['iso_types', 'internal_types']:
                continue
            if not kwargs.get(k, None):
                kwargs[k] = ''
                continue
            if k in ['args', 'c_args']:
                kwargs[k] = ', '.join(kwargs[k])
            elif k in ['args_def', 'args_conv_in', 'args_conv_out']:
                kwargs[k] = (
                    self.indent +
                    ('\n' + self.indent).join(kwargs[k]) + '\n')
            elif k in ['iso_types', 'internal_types']:
                pass
                # kwargs[k] = ', '.join(sorted(list(set(kwargs[k]))))
            else:
                kwargs[k] = ', '.join(list(set(kwargs[k])))
        for match in re.finditer(r'\(kind\s*=\s*(?P<kind>\w+)\)',
                                 kwargs['args_def']):
            if match.group('kind') in list(self.iso_types.values()):
                kwargs['iso_types'].append(match.group('kind'))
        for match in re.finditer(r'(?:^|\s)type\((?P<type>\w+)\)',
                                 kwargs['args_def']):
            if match.group('type') in list(self.iso_types.values()):
                kwargs['iso_types'].append(match.group('type'))
            elif match.group('type') in self.internal_types:
                kwargs['internal_types'].append(match.group('type'))
        if 'c_size_t' in kwargs['args_def']:
            assert kwargs['iso_types']
        for k in ['iso_types', 'internal_types']:
            kwargs[k] = ', '.join(sorted(list(set(kwargs[k]))))
        if kwargs.get('iso_types', None):
            kwargs['iso_binding'] = self.isodef.format(**kwargs)
        else:
            kwargs['iso_binding'] = ''
        if kwargs.get('internal_types', None):
            kwargs['import'] = self.importdef.format(**kwargs)
        else:
            kwargs['import'] = ''
        kwargs.setdefault('procedure', 'function')
        kwargs.setdefault('result', ' &\n     result(out)')
        body = self.wrapdef.format(**kwargs)
        return f'{docs}{body}'

    def parse_argument(self, x, kwargs):
        x.setdefault('is_input', False)
        x.setdefault('is_return', False)
        x.setdefault(
            'is_optional',
            (kwargs['function'] in self.optional_arguments
             and x['name'] in self.optional_arguments[kwargs['function']]))
        x.setdefault(
            'is_bypassed',
            (kwargs['function'] in self.bypassed_arguments
             and x['name'] in self.bypassed_arguments[kwargs['function']]))
        x.setdefault('is_modified', False)
        for k, v in self.modified_arguments.items():
            if kwargs['function'].startswith(k) and x['name'] in v:
                x['is_modified'] = v[x['name']]
        # Determine the raw_type
        if x['type'].startswith('const'):
            x['is_input'] = (not x['is_return'])
        self.update_type(x)
        # match = re.search(r'\(kind\s*=\s*(?P<kind>\w+)\)', x['type'])
        # if match and match.group('kind') in list(self.iso_types.values()):
        #     kwargs['iso_types'].append(match.group('kind'))
        # match = re.search(r'(?:^|\s)type\((?P<type>\w+)\)', x['type'])
        # if match:
        #     if match.group('type') in list(self.iso_types.values()):
        #         kwargs['iso_types'].append(match.group('type'))
        #     elif match.group('type') in self.internal_types:
        #         kwargs['internal_types'].append(match.group('type'))
        if x['is_return'] and (
                x['type'] == 'void'
                or (kwargs['check_function']
                    and not kwargs['check_function'].get('return', False))):
            kwargs['procedure'] = 'subroutine'
            kwargs['result'] = ''
            kwargs['c_result'] = 'call '
        elif not x['is_bypassed']:
            if x['is_return']:
                kwargs['ret_name'] = x['name']
            else:
                kwargs['args'].append(x['name'])
            kwargs['args_def'].append(x)

    def update_type(self, x):
        if x.get('is_c_arg', False) and not isinstance(self, FortranCdefsFile):
            return self.added['cdefs'].update_type(x)
        if x['raw_type'] in self.types['varient']:
            x['scalar_type'] = self.types['scalar'][x['scalar_raw_type']]
            x['scalar_base'] = x['scalar_type'].split("(")[0]
            x['scalar_type'] = x['scalar_type'].format(**x)
        x['type'] = self.types['all'][x['raw_type']].format(**x)
        if x['type'] in self.internal_types:
            x['type'] = f"type({x['type']})"
        assert 'const char*' not in x['type']
        assert x['type'] != 'int8_t'
        assert 'c_floatfloat_t' not in x['type']
        if x.get('is_c_arg', False) or isinstance(self, FortranCdefsFile):
            assert 'character(len = *)' not in x['type']

    def format_argsdef(self, x, just_type=False):
        if isinstance(x, list):
            return [self.format_argsdef(xx, just_type=just_type)
                    for xx in x]
        elif isinstance(x, str):
            return x
        assert isinstance(x, dict)
        self.update_type(x)
        x.setdefault('mods', [])
        if ((not x['raw_type'].startswith('type(')
             and not x['is_return']
             and 'character(' not in x['type']
             and x['is_arg'] and not x.get('is_c_arg', False)
             and 'target' not in x['mods']
             and 'pointer' not in x['mods'])):
            # TODO: Pass c_loc for c_ptr and then call c_f_pointer?
            x['mods'].append('value')
        if ((x['is_input'] and not x.get('is_c_arg', False)
             and 'target' not in x['mods'])):
            x['mods'].append('intent(in)')
        if x['is_optional']:
            x['mods'].append('optional')
        if x.get('mods', []):
            x['mods'] = ', ' + ', '.join(x['mods'])
        else:
            x['mods'] = ''
        assert x['mods'].strip() != ','
        if just_type:
            out = self.typedef.format(**x)
        else:
            out = self.argsdef.format(**x)
        assert '{length}' not in out
        assert "{x['length']}" not in out
        return out

    def add_conversion(self, a, b, kwargs, skip_optional=False):
        if ((a['is_return'] and kwargs['check_function']
             and not kwargs['check_function'].get('return', False))):
            return
        is_input = a['is_input']
        key = 'in' if a['is_input'] else 'out'
        # TODO: temp
        # pprint.pprint(kwargs)
        # print("FROM")
        # pprint.pprint(a)
        # print("TO")
        # pprint.pprint(b)
        equivalent = (
            a['raw_type'] == b['raw_type']
            and a.get('scalar_raw_type', '') == b.get('scalar_raw_type', ''))
        if a['is_optional'] and not skip_optional:
            cache = kwargs.pop('args_conv_in')
            kwargs['args_conv_in'] = []
            self.add_conversion(a, b, kwargs, skip_optional=True)
            default = CFile.param['default'].get(
                a['raw_type'], CFile.param['default']['default'])
            kws = dict(a,
                       present=('\n   ').join(
                           kwargs['args_conv_in']),
                       absent=f"{b['name']} = {default}")
            if a['raw_type'] in ['string']:
                if a['name'] in ['inFormat', 'outFormat']:
                    kws['absent'] = (
                        f"allocate({b['name']}(3))\n   "
                        + f"{b['name']}(1) = '%'\n   "
                        + f"{b['name']}(2) = 's'\n   "
                        + f"{b['name']}(3) = c_null_char")
                else:
                    kws['absent'] = (
                        f"allocate({b['name']}(1))\n   "
                        + f"{b['name']}(1) = c_null_char")
            kwargs['args_conv_in'] = cache
            kwargs['args_conv_in'] += self.optdef.format(
                **kws).splitlines()
        elif a['is_bypassed']:
            assert is_input
            rhs = self.bypassed_arguments[kwargs['function']][a['name']]
            kwargs['args_conv_in'].append(f"{b['name']} = {rhs}")
        else:
            if a.get('base_raw_type', False):
                if a['base_raw_type'] in ['noret']:
                    return
                conv = {
                    'a_update': {'raw_type': a['base_raw_type'],
                                 'mods': ['target']},
                    'args_def': (
                        '{a_type}, pointer :: {a_name}_ptr'),
                    'before': (
                        '{a_name}_ptr => {a_name}\n'
                        '{b_name} = c_loc({a_name}_ptr)'),
                    'after': (
                        'nullify({a_name}_ptr)'),
                }
                if (((kwargs['function'] == 'create_dtype_json_array'
                      and a['name'] == 'items')
                     or (kwargs['function'] == 'create_dtype_json_object'
                         and a['name'] == 'values'))):
                    conv.update(
                        args_def=('{a_type}, dimension(:), pointer :: '
                                  '{a_name}_ptr'))
            else:
                if (((not equivalent)
                     or a['raw_type'] not in self.conversions[key])):
                    raise NotImplementedError(
                        f"{kwargs['function']}: "
                        f"{a['raw_type']} vs. {b['raw_type']}")
                if ((a['raw_type'] == 'scalar'
                     and a.get('scalar_raw_type', 'invalid')
                     in self.conversions[key])):
                    conv = copy.deepcopy(
                        self.conversions[key][a['scalar_raw_type']])
                else:
                    conv = copy.deepcopy(
                        self.conversions[key][a['raw_type']])
                if conv.get('c2f', False):
                    conv.update(self.conversions[key]['c2f'])
            kws = copy.deepcopy(kwargs)
            # TODO: Set this correctly
            kws.setdefault('realloc', '.false.')
            for k, v in a.items():
                kws[f'a_{k}'] = v
            for k, v in b.items():
                kws[f'b_{k}'] = v
            if a['is_modified']:
                if isinstance(a['is_modified'], str):
                    if is_input:
                        a['is_modified'] = {'before': a['is_modified']}
                    else:
                        a['is_modified'] = {'after': a['is_modified']}
                for k, v in a['is_modified'].items():
                    if k in conv:
                        if k in ['a_update', 'b_update']:
                            for kk, vv in v.items():
                                if kk in ['mods']:
                                    conv[k].setdefault(kk, [])
                                    conv[k][kk] += vv
                                else:
                                    conv[k][kk] = vv
                        elif k in ['before', 'after', 'args_def']:
                            conv[k] = v
                            # conv[k] += '\n' + v
                        else:
                            raise NotImplementedError(k)
                    else:
                        conv[k] = v

            def update_x(name, x):
                xup = conv.get(f'{name}_update', {})
                for k, v in xup.items():
                    if k in ['mods']:
                        x.setdefault(k, [])
                        x[k] += v
                    else:
                        x[k] = v
                    kws[f'{name}_{k}'] = v
                self.update_type(x)
                kws[f'{name}_type'] = x['type']
                for k, v in xup.items():
                    if isinstance(v, str):
                        x[k] = x[k].format(**kws)

            # print("BEFORE")
            # print("A")
            # pprint.pprint(a)
            # print("B")
            # pprint.pprint(b)
            update_x('a', a)
            update_x('b', b)
            for k, kconv in zip(['args_conv_in', 'args_conv_out'],
                                ['before', 'after']):
                if conv.get(kconv, None):
                    kwargs[k] += conv[kconv].format(**kws).splitlines()
            if 'args_def' in conv:
                kwargs['args_def'] += conv['args_def'].format(
                    **kws).splitlines()
            # print("AFTER")
            # print("A")
            # pprint.pprint(a)
            # print("B")
            # pprint.pprint(b)
            # print("CONV")
            # pprint.pprint(conv)


class FortranCdefsFile(FortranFile):

    indent = '  '
    indent_append = '     '
    wrapdef = (
        '{procedure} {function}_c({args}){result} &\n'
        '     bind(c, name="{function}_f")\n'
        '{iso_binding}{import}'
        '  implicit none\n'
        '{args_def}'
        'end {procedure} {function}_c'
    )
    file_suffix = (
        '\n\n  end interface\n')
    excluded_functions = []
    checked_functions = {}
    optional_arguments = {}
    bypassed_arguments = {}
    modified_arguments = {}
    interface_docs = {}

    def __init__(self, *args, **kwargs):
        kwargs.update(cdefs=False, cwrap=False, finterface=False)
        super(FortranCdefsFile, self).__init__(*args, **kwargs)

    def init_param(self):
        self.types = copy.deepcopy(self.types)
        self.iso_types = copy.deepcopy(self.iso_types)
        self.types['core'].update(
            null='type(c_ptr)',
            boolean='logical(kind = c_bool)',
            integer='integer(kind = c_int)',
            number='real(kind = c_double)',
            string='character(kind = c_char), dimension({length})')
        self.types['scalar'].update(
            int='integer(kind = c_int{X}_t)',
            float='real(kind = c_{X_name})',
            complex='type(yggcomplex_{X_name})')
        self.types['util'].update(
            length='integer(kind = c_size_t)',
            shape='type(c_ptr)',
            shape_realloc='type(c_ptr)',
            flag='integer(kind = c_long)',
            flexible_string='type(c_ptr)',
            string_return='type(c_ptr)',
            string_realloc='type(c_ptr)',
            string_array='type(c_ptr)',
            commflag='integer(kind=c_int64_t)',
            timeout='integer(kind=c_int64_t)')
        self.types['varient'].update({
            '1darray': 'type(c_ptr)',
            'ndarray': 'type(c_ptr)'})
        # for k in ['bytes', 'unicode']:
        #     self.types['scalar'][k] = (
        #         'character(kind = c_char), dimension({length})')
        self.iso_types['char*'] = 'c_char'
        super(FortranCdefsFile, self).init_param()


def generate(**kwargs):
    CFile(fortran=FortranFile()).generate(**kwargs)
