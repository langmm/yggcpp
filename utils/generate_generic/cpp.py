import re
from generate_generic.base import (
    close_context, CodeUnit, TypeUnit, DocsUnit, VariableUnit,
    FunctionUnit, ClassUnit, MethodUnit, ConstructorUnit, DestructorUnit,
    ModuleUnit, FileUnit)


# PYTHONPATH=utils python -m generate_generic.runner --language=julia
#   --verbose --debug
# python utils/generate_generic.py --language=julia
# TODO:
# - Make template a unit
# - Handle separation of C/CXX header/source
# - Wrap enums


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
    member_units = ['macro', 'function']  # , 'var']


class CXXTemplateUnit(CodeUnit):

    unit_type = 'template'
    _properties = ['template_param']
    _regex = (
        r'template\s*\<\s*'
    )
    _fstring_cond = (
        'template<{template_param}>'
    )

    @classmethod
    def complete_match(cls, match, kwargs, member_units=None,
                       check_format=False):
        kwargs.setdefault('match_start', match.start())
        kwargs.setdefault('match_end', match.end())
        endpos_prev = kwargs['match_end']
        endpos = close_context("<", ">", match.string, pos=endpos_prev)
        kwargs['template_param'] = match.string[endpos_prev:endpos]
        kwargs['match_end'] = endpos


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
    def format_property(cls, k, x, **kwargs):
        if k == 'rhs':
            return f'={x}'
        return super(CXXVariableUnit, cls).format_property(k, x, **kwargs)


class CXXMethodUnit(MethodUnit):

    _regex_fstring = (
        # r'(?:\s*{docs}\s*)?'
        r'^(?P<indent>\s*)'
        r'(?:template\s*\<(?P<Tparam>.*?)\>\s+)?'
        r'{api}(?P<static>static\s+(?:inline\s+)?)?'
        r'(?P<virtual>virtual\s+)?'
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
        'body', 'Tparam', 'access', 'overloaded', 'static',
    ]
    member_context = ('{', '}')

    @property
    def address(self):
        return (f"{self.address_noargs}["
                f"{self.format_property('args', self.properties['args'])}]")

    @classmethod
    def parse_property(cls, k, x):
        out = super(CXXMethodUnit, cls).parse_property(k, x)
        if k == 'va_args':
            out = ', ' + out
        return out

    @classmethod
    def format_property(cls, k, x, **kwargs):
        if k == 'Tparam':
            return f'template<{x}>\n'
        out = super(CXXMethodUnit, cls).format_property(k, x, **kwargs)
        return out


class CXXConstructorUnit(ConstructorUnit):

    unit_type = 'constructor'
    _regex_fstring = (
        # r'^(?:\s*{docs}\s+)?'
        r'^(?P<indent>\s*){api}(?P<virtual>virtual\s+)?'
        r'(?P<explicit>explicit\s+)?'
        r'{R:parent}'
        r'(?P<args>\('
        r'(?:\s*{NG:var}\s*(?:\,))*'
        r'(?:\s*(?:(?:{NG:var})|(?P<va_args>\.\.\.))\s*)?'
        r'\))'
        r'\s*[;\{]'
    )
    _fstring_cond = (
        '{C:docs}'
        '{indent}{C:api}{C:virtual}{C:explicit}'
        '{parent}({args}{C:va_args});'
    )
    _properties = ['parent', 'args']
    _properties_optional = [
        'docs', 'api', 'virtual', 'va_args', 'body', 'access',
        'overloaded',
    ]


class CXXDestructorUnit(DestructorUnit):

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
        'docs', 'api', 'virtual', 'body', 'access',
    ]


class CXXClassUnit(ClassUnit):

    member_units = ['constructor', 'destructor', 'method']
    member_context = ('{', '}')
    _regex = (
        r'^(?P<indent>\s*)class\s+(?P<name>\w+)'
        r'(?P<base>\s*\:\s*(?P<base_scope>(?:public)|(?:private))\s+'
        r'(?P<base_class>[\:\w]+)'
        r'(?P<base_template>\s*\<\s*[^\{]+\s*\>)?'
        r')?'
        r'\s*\{'
    )
    _fstring_cond = (
        '{indent}class {name}{base} {\n'
        '{members}\n'
        '};'
    )
    _properties_optional = ['base', 'base_class']

    @classmethod
    def parse_subunits(cls, x, pos=None, endpos=None, **kwargs):
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(x)
        kwargs.update(pos=pos, endpos=endpos)
        full_pos = pos
        full_endpos = endpos
        access = None
        for match in super(CXXClassUnit, cls).parse_subunits(x, **kwargs):
            access = cls.check_access(match, x, pos, endpos,
                                      previous_access=access)
            # if access == 'public':
            yield match
            pos = match.match_end
            assert pos > full_pos
            assert pos < full_endpos
            assert match.match_start > full_pos
            assert match.match_start < full_endpos

    @classmethod
    def check_access(cls, match, x, pos=None, endpos=None,
                     previous_access=None):
        if 'access' in match.properties:
            return match.properties['access']
        if not x:
            match.properties['access'] = 'public'
            return match.properties['access']
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(x)
        if previous_access is None:
            previous_access = 'private'
        pattern = re.compile(
            r'^\s*(?P<access>(?:public)|(?:protected)|(?:private))'
            r'\:\s*$',
            flags=re.MULTILINE)
        matches = list([
            m for m in pattern.finditer(x, pos, match.match_start)])
        if matches:
            match.properties['access'] = matches[-1].group('access')
        else:
            match.properties['access'] = previous_access
        return match.properties['access']

    @classmethod
    def complete_match(cls, match, kwargs, **kws):
        out = super(CXXClassUnit, cls).complete_match(match, kwargs, **kws)
        counts = {}
        for x in kwargs.get('members', []):
            counts.setdefault(x.address_noargs, 0)
            counts[x.address_noargs] += 1
        for x in kwargs.get('members', []):
            x.properties.setdefault('overloaded',
                                    counts[x.address_noargs] > 1)
        kwargs['members'] = [
            x for x in kwargs.get('members', [])
            if x.properties['access'] == 'public']
        for x in kwargs.get('members', []):
            print(x, x.properties['access'], x.properties['overloaded'])
        return out


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

    language = 'cxx'
    ext = ['.hpp', '.cpp']
    member_units = ['macro', 'module', 'class', 'function']  # , 'var']
    modsep = '::'
