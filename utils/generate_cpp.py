from generate_generic import (
    CodeUnit, TypeUnit, DocsUnit, VariableUnit, FunctionUnit,
    ClassUnit, MethodUnit,
    GeneratedFile, close_context)


# TODO: Make template a unit


class CApiUnit(CodeUnit):

    additional_languages = ['cxx']
    unit_type = 'api'
    circular = True
    _regex = (
        r'(?P<api>YGG_API(?:\_DEF)?\s+)'
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


class CFile(GeneratedFile):
    r"""Class for generating/parsing C files."""

    ext = ['.c', '.h']
    comment = "//"
    indent = '  '
    member_units = ['function', 'var']


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
        r'\s*[;\{]\s*$'
    )
    _fstring_cond = (
        '{C:docs}'
        '{indent}{C:api}{C:virtual}{type} {name}({args}{C:va_args})'
        '{C:const}{C:override};'
    )
    _properties = ['name', 'type', 'args', 'parent']
    _properties_optional = [
        'api', 'virtual', 'const', 'override', 'va_args', 'Tparam'
    ]

    @property
    def address(self):
        return (f"{super(MethodUnit, self).address}["
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
        r'\s*[;\{]\s*$'
    )
    _properties_optional = [
        'api', 'virtual'
    ]


class CXXDestructorUnit(CodeUnit):

    unit_type = 'destructor'
    _regex_fstring = (
        # r'^(?:\s*{docs}\s+)?'
        r'^(?P<indent>\s*){api}(?P<virtual>virtual\s+)?'
        r'~{R:parent}\(\)'
        r'\s*[;\{]\s*$'
    )
    _properties_optional = [
        'api', 'virtual'
    ]


class CXXClassUnit(ClassUnit):

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
    member_units = ['constructor', 'method']
    _properties_optional = ['base']

    @classmethod
    def complete_match(cls, match, kwargs):
        endpos = close_context('{', '}', match.string, pos=match.end())
        kwargs.setdefault('match_end', endpos)
        return super(CXXClassUnit, cls).complete_match(match, kwargs)


class CXXFile(CFile):
    r"""Class for generating/parsing C++ files."""

    ext = ['.cpp', '.hpp']
    member_units = ['class', 'function']
