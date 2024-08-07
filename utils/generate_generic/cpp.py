from generate_generic.base import (
    CodeUnit, TypeUnit, DocsUnit, VariableUnit, FunctionUnit,
    ClassUnit, MethodUnit, ModuleUnit, FileUnit)


# PYTHONPATH=utils python -m generate_generic.runner --language=julia
# python utils/generate_generic.py --language=julia
# TODO:
# - Make template a unit
# - Handle separation of C/CXX header/source
# - Add module unit
# - Decorate C++ methods defined in header with YGG_API_DEF


class CApiUnit(CodeUnit):

    additional_languages = ['cxx']
    unit_type = 'api'
    circular = True
    _regex = (
        r'(?P<api>YGG_API(?:\_DEF)?\s+)'
    )


class CMacroUnit(CodeUnit):

    additional_languages = ['cxx']
    unit_type = 'macro'
    _properties = ['name']
    _properties_optional = ['macro_args', 'body']
    _regex = (
        r'^\#define\s+(?P<name>\w+)(?P<macro_args>\(.*?\))?'
        r'(?P<body>(?:.+?\\)*(?:.*))'
    )
    _fstring_cond = (
        '#define {name}{C:macro_args}{body}'
    )


class CTypeUnit(TypeUnit):

    _regex = (
        r'(?P<const>const\s+)?(?P<base>(?:[\w ])+)'
        r'(?P<ptr_space1>\s+)?(?P<ptr>\*+)?'
    )
    _fstring_cond = (
        r'{C:const}{base}{C:ptr}'
    )
    _properties = ['base']
    _properties_optional = ['const', 'ptr']


class CDocsUnit(DocsUnit):

    additional_languages = ['cxx']
    _regex = (
        r'(?:\/\*[\!\*])\s*'
        # r'(?P<docs>(?:.+?\s*(?:\n)?)+)' # \s*)+)'
        # r'(?P<docs>(?:(?:\s*\*\s*)?(?:[^\/]+\s*))+?)'
        r'(?P<docs>(?:[^\/])+?)'
        r'\s*\*\/'
    )


class CVariableUnit(VariableUnit):

    _regex_fstring = (
        r'^\s*(?P<type>{type})'
        r'(?P<ptr_space2>(?(ptr)(?(ptr_space1)(?:\s*)|(?:\s+))|(?:\s+)))'
        r'(\((?P<var_par>\*+)?)?(?P<name>\w+)(?(var_par)(?:\)))'
        r'(?P<shape>(?:\[.+?\])+)?'
        r'(?:\s*=\s*(?P<rhs>[^\;\,]+))?(?:(?:\;)|(?:\,)|(?:\)|(?:$)))'
    )
    _regex_nogroup = (
        r'(?:const\s+)?(?:[\w ]*\w)'
        r'(?:(?:\s+)|(?:\s+\*+\s*)|(?:\s*\*+\s+))'
        r'(?:(?:\w+)|(?:\(\s*\w+\s*\)))'
        r'(?:\[.+?\])*'
    )
    _fstring_cond = (
        r'{type} {name}{C:shape}'
    )
    _properties = ['name', 'type']
    _properties_optional = ['shape', 'rhs']


# class CVariableDecUnit(VariableUnit):

#     _regex_fstring = (
#         r'(?P<type>{type})'
#         r'(?P<ptr_space2>(?(ptr)(?(ptr_space1)(?:\s*)|(?:\s+))|(?:\s+)))'
#         r'{var}'
#     )
#     _properties = VariableUnit._properties + ['type']


class CFunctionUnit(FunctionUnit):

    additional_languages = ['cxx']
    _regex_fstring = (
        # r'^(?:\s*{docs}\s+)?'
        r'(?:\s*static inline\s*)?'
        r'^(?P<indent>\s*){api}(?P<type>{type})\s+(?P<name>\w+)'
        r'(?P<args>\('
        r'(?:\s*{NG:var}\s*(?:\,))*'
        r'(?:\s*(?:(?:{NG:var})|(?P<va_args>\.\.\.))\s*)?'
        r'\))\s*[;\{]\s*$'
    )
    _fstring_cond = (
        r'{indent}{C:api}{type} {name}({args});'
    )
    _properties = FunctionUnit._properties + ['type']
    _properties_optional = ['api']
    member_context = ('{', '}')


class CFileUnit(FileUnit):
    r"""Class for generating/parsing C files."""

    ext = ['.h', '.c']
    comment = "//"
    indent = '  '
    member_units = ['macro', 'function', 'var']


class CXXTypeUnit(TypeUnit):

    _regex = (
        r'(?P<const>const\s+)?(?P<base>(?:[\w\: ])+)'
        r'(?P<ptr_space1>\s+)?(?P<ptr>\*+)?(?P<ref>\&)?'
    )
    _fstring_cond = (
        r'{C:const}{base}{C:ptr}{C:ref}'
    )
    _properties = ['base']
    _properties_optional = ['const', 'ptr', 'ref']


class CXXVariableUnit(VariableUnit):

    _regex_fstring = (
        r'\s*(?P<type>{type})'
        r'(?P<ptr_space2>(?(ptr)(?(ptr_space1)(?:\s*)|(?:\s+))|(?:\s+)))'
        r'(\((?P<var_par>[\*\&]+)?)?(?P<name>\w+)(?(var_par)(?:\)))'
        r'(?P<shape>(?:\[.+?\])+)?'
        r'(?:\s*=\s*(?P<rhs>[^\;\,\)]+))?'
        r'(?:(?:\;)|(?:\,)|(?:\))|(?:$))'
    )
    _regex_nogroup = (
        r'(?:const\s+)?(?:[\w\: ]*\w)'
        r'(?:(?:\s*)|(?:\s+\*+\s*)|(?:\s*\*+\s+))(?:\&\s+)?'
        r'(?:(?:\w+)|(?:\(\s*\w+\s*\)))'
        r'(?:\[.+?\])*'
        r'(?:\s*=\s*(?:[^\;\,\)]+))?'
    )
    _fstring_cond = (
        r'{type} {name}{C:shape}{C:rhs}'
    )
    _properties = ['name', 'type']
    _properties_optional = ['shape', 'rhs']

    @classmethod
    def format_property(cls, k, x):
        if k == 'rhs':
            return f'={x}'
        return super(CXXVariableUnit, cls).format_property(k, x)


class CXXMethodUnit(MethodUnit):

    _regex_fstring = (
        # r'(?:\s*{docs}\s*)?'
        r'(?:\s*(?:static inline)|(?:template\<(?P<Tparam>.*?)\>)\s*)?'
        r'^(?P<indent>\s*){api}(?P<virtual>virtual\s+)?'
        r'(?P<type>{NG:type})\s+(?P<name>\w+)'
        r'(?P<args>\('
        r'(?:\s*{NG:var}\s*(?:\,))*'
        r'(?:\s*(?:(?:{NG:var})|(?P<va_args>\.\.\.))\s*)?'
        r'\))(?P<const>\s+const)?'
        r'(?P<override>\s+(?:(?:override)|(?:VIRT\_END)))?'
        r'\s*[;\{]'
    )
    _fstring_cond = (
        '{C:docs}'
        '{indent}{C:api}{C:virtual}{type} {name}({args}{C:va_args})'
        '{C:const}{C:override};'
    )
    _properties = ['name', 'type', 'args', 'parent']
    _properties_optional = [
        'docs', 'api', 'virtual', 'const', 'override', 'va_args',
        'body', 'Tparam',
    ]
    member_context = ('{', '}')

    @property
    def address(self):
        return (f"{super(CXXMethodUnit, self).address}["
                f"{self.format_property('args', self.properties['args'])}]")

    @classmethod
    def parse_property(cls, k, x):
        out = super(CXXMethodUnit, cls).parse_property(k, x)
        if k == 'va_args':
            out = ', ' + out
        return out

    @classmethod
    def format_property(cls, k, x):
        if k == 'Tparam':
            return f'template<{x}>\n'
        out = super(CXXMethodUnit, cls).format_property(k, x)
        return out


class CXXConstructorUnit(CodeUnit):

    unit_type = 'constructor'
    _regex_fstring = (
        # r'^(?:\s*{docs}\s+)?'
        r'^(?P<indent>\s*){api}(?P<virtual>virtual\s+)?'
        r'{R:parent}'
        r'(?P<args>\('
        r'(?:\s*{NG:var}\s*(?:\,))*'
        r'(?:\s*(?:(?:{NG:var})|(?P<va_args>\.\.\.))\s*)?'
        r'\))'
        r'\s*[;\{]'
    )
    _fstring_cond = (
        '{C:docs}'
        '{indent}{C:api}{C:virtual}{parent}({args}{C:va_args});'
    )
    _properties = ['parent', 'args']
    _properties_optional = [
        'docs', 'api', 'virtual', 'va_args', 'body'
    ]


class CXXDestructorUnit(CodeUnit):

    unit_type = 'destructor'
    _regex_fstring = (
        # r'^(?:\s*{docs}\s+)?'
        r'^(?P<indent>\s*){api}(?P<virtual>virtual\s+)?'
        r'~{R:W:parent}\(\)'
        r'\s*[;\{]'
    )
    _fstring_cond = (
        '{C:docs}'
        '{indent}{C:api}{C:virtual}~{parent}();'
    )
    _properties = ['parent']
    _properties_optional = [
        'docs', 'api', 'virtual', 'body'
    ]


class CXXClassUnit(ClassUnit):

    member_units = ['constructor', 'destructor', 'method']
    member_context = ('{', '}')
    _regex = (
        r'^(?P<indent>\s*)class\s+(?P<name>\w+)'
        r'(?P<base>\s*\:\s+(?P<base_scope>(?:public)|(?:private))\s+'
        r'(?P<base_name>[\:\w]+))?'
        r'\s*\{'
    )
    _fstring_cond = (
        '{indent}class {name}{base} {\n'
        '{members}\n'
        '};'
    )
    _properties_optional = ['base']


class CXXModuleUnit(ModuleUnit):

    member_units = ['module', 'class', 'function']  # , 'var']
    member_context = ('{', '}')
    _regex = (
        r'^(?P<indent>\s*)namespace\s+(?P<name>\w+)\s+\{'
    )
    _fstring_cond = (
        '{indent}namespace {name} {\n'
        '{members}\n'
        '}'
    )


class CXXFileUnit(CFileUnit):
    r"""Class for generating/parsing C++ files."""

    ext = ['.hpp', '.cpp']
    member_units = ['macro', 'class', 'function']  # , 'var']
