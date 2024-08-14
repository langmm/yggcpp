import re
import copy
from generate_generic.base import (
    ContextToken, NoDefault, CodeUnit,
    EnumValueUnit, EnumUnit, TypeUnit, DocsUnit,
    VariableUnit, FunctionUnit, ClassUnit,
    DestructorUnit, ModuleUnit, ImportUnit, FileUnit)


class CApiUnit(CodeUnit):

    language = 'c'
    additional_languages = ['cxx']
    unit_type = 'api'
    circular = True
    _regex_fstring = (
        r'(?P<api>YGG_API(?:\_DEF)?\s+)'
    )
    optional = True


class CMacroUnit(CodeUnit):

    language = 'c'
    additional_languages = ['cxx']
    unit_type = 'macro'
    allow_duplicates = True
    _properties = ['name']
    _properties_optional = ['macro_args', 'body']
    _regex = (
        r'^\#define\s+(?P<name>\w+)(?P<macro_args>\([\w \,]?\))?'
        r'(?P<body>(?:[^\\\n]+?\\\s*$)*(?:[^\\\n]*$))'
    )
    _fstring_cond = (
        '#define {name}{macro_args}{body}'
    )


class CPreprocessContextUnit(CodeUnit):

    language = 'c'
    additional_languages = ['cxx']
    unit_type = 'preprocess_context'
    _regex = (
        r'^\s*#if(?P<name>.*)$'
    )
    _fstring_cond = (
        '#if{name}'
    )


class CEnumValueUnit(EnumValueUnit):

    additional_languages = ['cxx']
    _regex = (
        r'^(?P<indent>\s*)(?P<name>\w+)\s*'
        r'(?:=\s*(?P<value>\w+)\s*)?(?:\,\s*)?'
    )
    _fstring_cond = (
        '{indent}{name} {PREFIX[= ]:value},'
    )
    _properties_optional = EnumValueUnit._properties_optional + [
        'value'
    ]


class CEnumUnit(EnumUnit):

    additional_languages = ['cxx']
    _properties_optional = EnumUnit._properties_optional + ['type']
    member_context = ('{', '}')
    _regex = (
        r'^(?P<indent>\s*)enum\s+(?P<name>\w+)\s*'
        r'(?:\s*\:\s*(?P<type>\w+)\s*)?'
        r'\s*\{'
    )
    _fstring_cond = (
        '{indent}enum {name} {\n'
        '{members}\n'
        '};'
    )


class CTypeBaseUnit(CodeUnit):

    language = 'c'
    unit_type = 'base_type'
    _regex = (
        r'(?:(?:(?:(?:(?:(?:un)?signed)|(?:long)|(?:short)) )+'
        r'(?:(?:int)|(?:double)|(?:char)|'
        r'(?:unsigned)|(?:long)|(?:short)))|'
        r'(?:(?!return)\w+))'
    )


class CTypeUnit(TypeUnit):

    _regex_fstring = (
        r'(?P<const>const\s+)?(?P<enum>enum\s+)?(?P<base>{base_type})'
        r'(?:(?P<ptr_space1>\s+)?(?P<ptr>[\*]+))?'
    )
    _regex_nogroup_fstring = (
        r'(?:const\s+)?(?:enum\s+)?(?:{NG:base_type})'
        r'(?:\s*[\*]+)?'
    )
    _fstring_cond = (
        r'{const}{enum}{base}{ptr}'
    )
    _properties = ['base']
    _properties_optional = [
        'const', 'enum', 'ptr', 'ptr_space1', 'item_ptr', 'shape'
    ]
    address_property = 'base'

    def utilized_type_units(self, **kwargs):
        out = [self.parse(self.properties['base'])]
        out += super(CTypeUnit, self).utilized_type_units(**kwargs)
        return out

    def replace_type(self, a, b, **kwargs):
        if self.properties['base'] == a:
            self.set_property('base', b)
        super(CTypeUnit, self).replace_type(a, b, **kwargs)


class CDocsUnit(DocsUnit):

    additional_languages = ['cxx']
    _regex = (
        r'(?:\/\*[\!\*])\s*'
        # r'(?P<docs>(?:.+?\s*(?:\n)?)+)' # \s*)+)'
        # r'(?P<docs>(?:(?:\s*\*\s*)?(?:[^\/]+\s*))+?)'
        r'(?P<docs>(?:[^\/])+?)'
        r'\s*\*\/'
    )

    def replace_type(self, a, b, **kwargs):
        self.set_property(
            'docs', self.properties['docs'].replace(a, b))
        super(CDocsUnit, self).replace_type(a, b, **kwargs)


class CVariableUnit(VariableUnit):

    _regex_fstring = (
        r'\s*(?P<type>{type})'
        r'(?:'
        r'(?P<ptr_space2>(?(ptr_space1)(?:\s*)|(?:\s*)))'
        r'(?P<type_paren>\(\s*(?P<ptr2>[\*]+\s*)?)?'
        r'(?P<name>\w+)(?(type_paren)(?:\s*\)))'
        r'(?P<shape>(?:\[.*\])+)?)?\s*'
    )
    _regex_nogroup_fstring = (
        CTypeUnit.get_regex(no_group=True, return_fstring=True)
        + r'(?:\s*(?:(?:\w+)|(?:\([\*\s]*\w+\s*\)))'
        + r'(?:\[.*\])*)?\s*'
    )
    _fstring_cond = (
        r'{type} ({ptr2}{name}){shape}'
    )
    _properties = ['name', 'type']
    _properties_optional = [
        'shape', 'ptr2', 'ptr_space2', 'type_paren'
    ]
    _properties_defaults = dict(
        VariableUnit._properties_defaults,
        name='UNNAMED')


class CFunctionUnit(FunctionUnit):

    _regex_fstring = (
        # r'(?:\s*{docs}\s*)?'
        r'^(?P<indent>\s*)'
        r'{api}(?P<static>static\s+(?:inline\s+)?)?'
        r'(?P<retmacro>RAPIDJSON\_(?:(?:DIS)|(?:EN))ABLEIF\_RETURN\('
        r'\s*\(.+?\)\s*\,\s*\(\s*)?'
        r'(?P<type>{NG:type})'
        r'(?(retmacro)(?:\s*\)\s*\)))?'
        r'\s+(?P<name>(?!operator)\w+)'
        r'(?:\((?P<args>(?:'
        r'(?:\s*{NG:var}\s*(?:\,))*\s*{NG:var})?)'
        r'(?:(?:\s*\,)?'
        r'(?P<va_args>\.\.\.)|'
        r'(?P<argmacro>RAPIDJSON\_(?:(?:DIS)|(?:EN))ABLEIF\(.+?\))'
        r')?\s*'
        r'\))'
        r'(?:\s+\w+)?'
        r'\s*[;\{]'
    )
    _fstring_cond = (
        '{SUFFIX[\\n]:preprocess_contexts}'
        '{docs}'
        '{indent}{api}{static}{type} {name}({args}{va_args});'
        '{PREFIX[\\n]:RITER[#endif]:preprocess_contexts}'
    )
    _properties = FunctionUnit._properties + [
        'type'
    ]
    _properties_optional = FunctionUnit._properties_optional + [
        'docs', 'api', 'static', 'va_args', 'body',
        'preprocess_contexts', 'rapidjson_type',
        'retmacro', 'argmacro',
    ]
    member_context = ('{', '}')
    property_subunits = {
        'args': 'var',
        'preprocess_contexts': 'preprocess_context',
    }
    list_seps = dict(
        FunctionUnit.list_seps,
        preprocess_context='',
    )

    @property
    def address(self):
        out = super(CFunctionUnit, self).address
        if self.properties.get('template', None):
            out += self.format_property(
                'template', self.properties['template'])
        out += f"[{self.format_property('args', self.properties['args'])}]"
        if self.properties.get('const_method', None):
            out += " const "
        if 'type' in self.properties:
            out += " -> "
            out += self.format_property('type', self.properties['type'])
        return out

    def check_duplicate(self, solf):
        for k in ['retmacro', 'argmacro']:
            if self.get_property(k, None) != solf.get_property(k, None):
                return False
        return super(CFunctionUnit, self).check_duplicate(solf)

    def get_property(self, k, default=NoDefault, **kwargs):
        if ((k == 'preprocess_contexts' and k not in kwargs
             and k not in self.properties)):
            file_unit = self.get_property('file_unit', None)
            tokens = file_unit.get_property('preprocess_tokens',
                                            default=None,
                                            **kwargs)
            out = None
            containers_parent = None
            containers = None
            if tokens and self.match:
                pos = tokens.adjust_position(
                    self.match.start(), check_against=self.match.string)
                endpos = tokens.adjust_position(
                    self.match.end(), check_against=self.match.string)
                parent_pos = tokens.adjust_position(
                    self.parent_unit.match.start())
                parent_endpos = tokens.adjust_position(
                    self.parent_unit.match.end())
                containers_parent = tokens.tokens_containing(
                    pos, endpos, include_tokens=['preprocess'])
                containers = [
                    x for x in containers_parent
                    if not x.contains(parent_pos, parent_endpos)]
                preunit_file = file_unit.code_unit('preprocess_context')
                out = [preunit_file.parse(x.group()) for x in containers]
                out = [
                    x for x in out if x.get_property('name') not in
                    file_unit.get_property(
                        'ignored_preprocess_contexts', [])
                ]
            if out:
                self.set_property(k, out)
                return out
            # elif ((self.get_property('name', None) == 'SetUnits'
            #        and len(self.properties['args']) == 1)):
            #     import pprint
            #     pprint.pprint(self.properties)
            #     print("NO PREPROCESS", tokens, self.match_start,
            #           self.match_end, len(self.properties['args']))
            #     print(containers)
            #     print(containers_parent)
            #     import pdb
            #     pdb.set_trace()
        kwargs['default'] = default
        return super(CFunctionUnit, self).get_property(k, **kwargs)


class CImportUnit(ImportUnit):

    language = 'c'
    additional_languages = ['cxx']
    allow_duplicates = 'ignore'
    _properties_optional = ImportUnit._properties_optional + [
        "local_ctx", "global_ctx",
    ]
    _regex = (
        r'^\#include\s+'
        r'(?P<local_ctx>\")?(?P<global_ctx>\<)?'
        r'(?P<name>.+)'
        r'(?(global_ctx)\>)(?(local_ctx)\")'
        r'\s*$'
    )
    _fstring_cond = (
        '#include {local_ctx}{global_ctx}{name}'
        '{IF:{global_ctx}:STR[>]:0}{local_ctx}'
    )


class CFileUnit(FileUnit):
    r"""Class for generating/parsing C files."""

    ext = ['.h', '.c']
    comment = "//"
    block_comment = ("/*", "*/")
    indent = '  '
    member_units = ['macro', 'function']
    context_tokens = {
        'preprocess': ContextToken(
            r"^\s*#if.*$", r"^\s*#endif.*$",
            name='preprocess', use_regex=True,
            subtokens=[r'^\s*(?:(?:#else)|(?:#elif).*$)']),
    }
    _properties_optional = FileUnit._properties_optional + [
        'preprocess_tokens', 'ignored_preprocess_contexts',
    ]

    @classmethod
    def preprocess(cls, x):
        x, kws = super(CFileUnit, cls).preprocess(x)
        tokens = [token for token in cls.context_tokens.values() if
                  token.exclusive]
        out, match = cls.context_tokens['preprocess'].removeall(
            x, skip_tokens=tokens, return_match=True)
        # check_for=["#if", "#endif"],
        # check_adjusted=True, verbose=True)
        if match:
            kws['preprocess_tokens'] = match
        return out, kws


class CXXTemplateParamUnit(CodeUnit):

    language = 'cxx'
    unit_type = 'template_param'
    _properties = ['name', 'param_type']
    _properties_optional = ['default']
    _regex_fstring = (
        r'(?P<param_type>(?:typename)|(?:\w+))'
        r'\s+(?P<name>\w+)(?:\s*\=\s*'
        r'(?P<default>(?:\w+)|(?:{NG:type})))?'
    )
    _fstring_cond = (
        r'{param_type} {name}{PREFIX[=]:default}'
    )

    def utilized_type_units(self, **kwargs):
        out = super(CXXTemplateParamUnit, self).utilized_type_units(
            **kwargs)
        if self.properties['param_type'] == 'typename':
            if 'default' in self.properties:
                out.append(CXXTypeUnit.parse(self.properties['default']))
        else:
            out.append(CXXTypeUnit.parse(self.properties['param_type']))
        return out

    def replace_type(self, a, b, **kwargs):
        if self.properties['param_type'] == 'typename':
            if 'default' in self.properties:
                self.set_property(
                    'default', self.properties['default'].replace(a, b))
        else:
            self.set_property(
                'param_type', self.properties['param_type'].replace(a, b))
        super(CXXTemplateParamUnit, self).replace_type(a, b, **kwargs)


class CXXTemplateUnit(CodeUnit):

    language = 'cxx'
    unit_type = 'template'
    _properties = ['param']
    _regex_fstring = (
        r'template\s*{RCONTAINER:param}'
    )
    _regex_nogroup = (
        r'template\s*\<\s*.+\s*\>'
    )
    _fstring_cond = (
        'template< {param} >'
    )
    list_seps = {
        'param': ', ',
    }
    list_bounds = {
        'param': ('<', '>'),
    }
    property_subunits = {
        'param': 'template_param',
    }


class CXXTypeBaseUnit(CodeUnit):

    language = 'cxx'
    unit_type = 'base_type'
    _regex = CTypeBaseUnit._regex.replace(
        r'\w+',
        r'(?P<base_module>(?:\w+(?:\<.*?\>)?\:\:)*)\w+'
    )
    _ptr_replacements = [
        (r'[\*]', r'[\*\&]'),
        (r'[\*\s]', r'[\*\&\s]'),
    ]

    @classmethod
    def _replace_ptr(cls, out):
        for x, y in cls._ptr_replacements:
            out = out.replace(x, y)
        return out


class CXXTypeUnit(CTypeUnit):

    language = 'cxx'
    _regex_fstring = CXXTypeBaseUnit._replace_ptr(
        CTypeUnit._regex_fstring.replace(
            r'(?P<base>{base_type})',
            r'(?P<typename>typename\s+)?(?P<base>{base_type})'
            r'{CONTAINER:template_spec}')
    )
    _regex_norecurse_fstring = '(?:{NG:base_type})'
    _regex_nogroup_fstring = None
    _fstring_cond = CTypeUnit._fstring_cond.replace(
        r'{base}', r'{typename}{base}{BOOKEND:template_spec}')
    _properties_optional = CTypeUnit._properties_optional + [
        'template_spec', 'base_module', 'typename',
    ]
    property_subunits = {
        'template_spec': 'type',
    }

    def specialize(self, name, spec, **kwargs):
        if self.properties['base'] == name:
            assert isinstance(spec, list)
            assert all(isinstance(x, CXXTypeUnit) for x in spec)
            self.properties['template_spec'] = spec
            # if new_name:
            #     self.set_property('name', new_name)
        super(CXXTypeUnit, self).specialize(name, spec, **kwargs)


class CXXVariableUnit(CVariableUnit):

    language = 'cxx'
    _regex_fstring = CXXTypeBaseUnit._replace_ptr(
        CVariableUnit._regex_fstring
        + r'(?:\s*\=\s*(?P<rhs>.+))?'
    )
    _regex_nogroup_fstring = CXXTypeBaseUnit._replace_ptr(
        CVariableUnit.get_regex(no_group=True,
                                return_fstring=True).replace(
            CTypeUnit.get_regex(no_group=True, return_fstring=True),
            CXXTypeUnit.get_regex(no_group=True, return_fstring=True))
        + r'(?:\s*\=\s*.+)?'
    )
    _fstring_cond = (
        CVariableUnit._fstring_cond
        + r'{PREFIX[=]:rhs}'
    )
    _properties_optional = CVariableUnit._properties_optional + [
        'rhs', 'is_output',
    ]
    property_subunits = copy.deepcopy(
        CXXTypeUnit.property_subunits)

    def __init__(self, *args, **kwargs):
        super(CXXVariableUnit, self).__init__(*args, **kwargs)
        if 'rhs' in self.properties:
            self.properties['rhs'] = self.properties['rhs'].rstrip(',')

    def replace_type(self, a, b, **kwargs):
        if 'rhs' in self.properties:
            self.properties['rhs'] = self.properties['rhs'].replace(a, b)
        super(CXXVariableUnit, self).replace_type(a, b, **kwargs)


class CXXFunctionUnit(CFunctionUnit):

    language = 'cxx'
    _regex_fstring = CFunctionUnit._regex_fstring.replace(
        r'^(?P<indent>\s*)',
        r'(?P<template>^\s*{template}\s*)?^(?P<indent>\s*)')
    _fstring_cond = CFunctionUnit._fstring_cond.replace(
        '{indent}',
        '{indent}{template}\n{indent}')
    _properties_optional = CFunctionUnit._properties_optional + [
        'template'
    ]


class CXXMethodUnit(CXXFunctionUnit):

    unit_type = 'method'
    _regex_fstring = CXXFunctionUnit._regex_fstring.replace(
        r'(?:\s+\w+)?\s*[;\{]',
        r'(?P<const_method>\s+const)?'
        r'(?P<override>\s+(?:(?:override)|(?:VIRT\_END)))?'
        r'(?:\s+\w+)?\s*[;\{]').replace(
            r'(?P<static>static\s+(?:inline\s+)?)?',
            r'(?P<static>static\s+(?:inline\s+)?)?'
            r'(?P<virtual>virtual\s+)?'
            r'(?P<friend>friend\s+)?')
    _fstring_cond = CXXFunctionUnit._fstring_cond.replace(
        ';',
        '{const_method}{override};').replace(
            '{api}',
            '{api}{virtual}{friend}')
    _properties = CXXFunctionUnit._properties + [
        'parent'
    ]
    _properties_optional = CXXFunctionUnit._properties_optional + [
        'const_method', 'override', 'virtual', 'overloaded', 'access',
        'friend',
    ]

    @classmethod
    def parse_property(cls, k, x):
        out = super(CXXMethodUnit, cls).parse_property(k, x)
        if k == 'va_args':
            out = ', ' + out
        return out


class CXXOperatorUnit(CXXMethodUnit):

    unit_type = 'operator'
    _regex_fstring = CXXMethodUnit._regex_fstring.replace(
        r'(?P<name>(?!operator)\w+)',
        r'operator(?P<name>[\(\)\[\]\+\-\=\\\*\%\<\>\w]+)\s*')
    _fstring_cond = CXXMethodUnit._fstring_cond.replace(
        '{name}', 'operator{name}')


class CXXConstructorUnit(CXXMethodUnit):

    unit_type = 'constructor'
    _regex_fstring = CXXMethodUnit._regex_fstring.replace(
        r'(?P<retmacro>RAPIDJSON\_(?:(?:DIS)|(?:EN))ABLEIF\_RETURN\('
        r'\s*\(.+?\)\s*\,\s*\(\s*)?'
        r'(?P<type>{NG:type})'
        r'(?(retmacro)(?:\s*\)\s*\)))?\s+',
        r'(?P<explicit>explicit\s+)?').replace(
            r'(?P<name>(?!operator)\w+)', r'{KWS:parent}').replace(
                r'(?P<static>static\s+(?:inline\s+)?)?', '').replace(
                    r'[;\{]', r'(?P<base_init>\:\s*[^;\{]+)?[;\{]')
    _fstring_cond = CXXMethodUnit._fstring_cond.replace(
        '{type} ', '{explicit}').replace(
            '{static}', '').replace(
                '{friend}', '').replace(
                    '{name}', '{parent}').replace(
                        '{const_method}{override}', '')
    _properties = ['parent'] + [
        x for x in CXXMethodUnit._properties
        if x not in ['name', 'type', 'parent']]
    _properties_optional = [
        x for x in CXXMethodUnit._properties_optional
        if x not in ['const', 'override', 'static', 'friend']] + [
                'explicit'
        ]
    address_property = 'parent'


class CXXDestructorUnit(DestructorUnit):

    unit_type = 'destructor'
    member_context = ('{', '}')
    _regex_fstring = (
        # r'^(?:\s*{docs}\s+)?'
        r'^(?P<indent>\s*){api}(?P<virtual>virtual\s+)?'
        r'~{KWS[\w+]:parent}\(\)'
        r'\s*[;\{]'
    )
    _fstring_cond = (
        '{docs}'
        '{indent}{api}{virtual}~{parent}();'
    )
    _properties = ['parent']
    _properties_optional = [
        'docs', 'api', 'virtual', 'body', 'access',
    ]
    address_property = 'parent'


class CXXClassUnit(ClassUnit):

    member_units = ['constructor', 'destructor', 'method', 'operator']
    member_context = ('{', '}')
    members_discontiguous = True
    _regex_fstring = (
        r'(?P<template>^\s*{template}\s*)?'
        r'^(?P<indent>\s*)class\s+(?P<name>\w+)'
        r'(?P<base>\s*\:\s*(?P<base_scope>(?:public)|(?:private))\s+'
        r'(?P<base_class>[\:\w]+)'
        r'{CONTAINER:base_template_spec}'
        r')?'
        r'\s*\{'
    )
    _fstring_cond = (
        '{indent}{template}class {name}{base}'
        '{BOOKEND:base_template_spec} {\n'
        '{members}\n'
        '};'
    )
    _properties_optional = [
        'macro', 'base', 'base_class', 'base_class_unit',
        'template', 'template_spec',
        'base_template_spec', 'template_param'
    ]
    property_subunits = {
        'base_template_spec': 'type',
    }

    def utilized_type_units(self, **kwargs):
        out = super(CXXClassUnit, self).utilized_type_units(**kwargs)
        for k in ['base', 'base_class']:
            if k in self.properties:
                out.append(CXXTypeUnit.parse(self.properties[k]))
        return out

    def replace_type(self, a, b, **kwargs):
        if self.properties['name'] == a:
            self.set_property('name', b)
        for k in ['base', 'base_class']:
            if k in self.properties:
                self.properties[k] = self.properties[k].replace(a, b)
        super(CXXClassUnit, self).replace_type(a, b, **kwargs)

    def specialize(self, name, spec, new_name=None, **kwargs):
        if name == self.properties['name']:
            assert 'template' in self.properties
            self.properties['template_spec'] = spec
            if new_name:
                self.set_property('name', new_name)
        super(CXXClassUnit, self).specialize(name, spec, **kwargs)

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
            yield match
            pos = match.match_end
            if pos > full_endpos:
                print(match)
                print(full_pos, full_endpos)
                print(pos, endpos)
                import pdb
                pdb.set_trace()
            assert pos > full_pos
            assert pos <= full_endpos
            assert match.match_start >= full_pos
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
            counts.setdefault(x.address_stripped, 0)
            counts[x.address_stripped] += 1
        for x in kwargs.get('members', []):
            x.properties.setdefault('overloaded',
                                    counts[x.address_stripped] > 1)
        kwargs['members'] = [
            x for x in kwargs.get('members', [])
            if x.properties['access'] == 'public']
        return out


class CXXStructUnit(CXXClassUnit):

    allow_duplicates = True
    unit_type = 'struct'
    _regex_fstring = CXXClassUnit._regex_fstring.replace(
        r'class\s+', r'struct\s+').replace(
            r'(?P<name>\w+)',
            r'(?P<name>\w+){CONTAINER:template_spec}')
    _fstring_cond = CXXClassUnit._fstring_cond.replace(
        'class ', 'struct ').replace(
            '{name}',
            '{name}{BOOKEND:template_spec}')
    _properties_optional = CXXClassUnit._properties_optional + [
        'template_spec'
    ]
    property_subunits = dict(
        CXXClassUnit.property_subunits,
        **CXXTypeUnit.property_subunits
    )

    @property
    def address(self):
        out = super(CXXStructUnit, self).address
        if self.get_property('template_spec', None):
            out += (
                "<" + self.format_property(
                    'template_spec', self.get_property('template_spec'))
                + ">")
        return out


class CXXModuleUnit(ModuleUnit):

    allow_duplicates = True
    member_units = [
        'macro', 'import', 'module', 'class', 'function', 'enum',
        'struct',
    ]
    member_context = ('{', '}')
    members_discontiguous = True
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
    member_units = ['macro'] + CXXModuleUnit.member_units
    members_discontiguous = True
    modsep = '::'
