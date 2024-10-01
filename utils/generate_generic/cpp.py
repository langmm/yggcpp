import re
import pprint
import itertools
from generate_generic.base import (
    ContextToken, NoDefault, CodeUnit,
    EnumValueUnit, EnumUnit, TypeUnit, DocsUnit,
    VariableUnit, FunctionUnit, ClassUnit,
    DestructorUnit, ModuleUnit, ImportUnit, FileUnit)


_token_sep_OK = [
    '.', ' ', '\t', '\n', '<', '>', '[', ']', '(', ')', '{', '}',
    '*', '&', '^', '+', '-', '/', '!', '%', ';', ':', '~', ',', '|',
    '\\',
]


def do_replace_type_str(a, b, x, exact=False, use_name=False):
    x0 = x
    rep = {}
    if use_name:
        a_name = (a.properties['base'].properties['name']
                  if isinstance(a, CodeUnit) else a)
        b_name = (b.properties['base'].properties['name']
                  if isinstance(b, CodeUnit) else b)
        rep[a_name] = b_name
    else:
        a_name = a.format() if isinstance(a, CodeUnit) else a
        b_name = b.format() if isinstance(b, CodeUnit) else b
        rep[a_name] = b_name
        if isinstance(a, CodeUnit):
            rep[a.properties['base'].properties['name']] = b_name
    # xalt = x
    for k, v in rep.items():
        if exact:
            if x == k:
                x = v
        else:
            idx = 0
            while k in x[idx:]:
                idx = x.find(k, idx)
                idxend = idx + len(k)
                prefixOK = (
                    idx == 0 or x[idx - 1] in _token_sep_OK)
                suffixOK = (
                    idxend == len(x) or
                    x[idxend] in _token_sep_OK
                )
                if prefixOK and suffixOK:
                    x = x[:idx] + v + x[idxend:]
                    idx = (idx + len(v))
                else:
                    idx = idxend
            # xalt = xalt.replace(k, v)
            # if x != xalt:
            #     print(f"In replacing \"{k}\" with \"{v}\", "
            #           f"new version differs:\n\t{x}\n\t{xalt}")
            #     import pdb; pdb.set_trace()
    if x == x0:
        return None
    return x


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
    no_forward_unit = True
    _properties = ['name']
    _properties_optional = ['macro_args', 'body']
    _regex = (
        r'^[\t ]*\#define[\t ]+(?P<name>\w+)(?P<macro_args>\('
        r'(?:[\t ]*\w+\,[\t ]*)*(?:[\t ]*(?:(?:\w+)|(?:\.\.\.))[\t ]*)?'
        r'\))?'
        r'(?P<body>'
        r'(?:[^\\\n]+\\\n)*'
        r'(?:[^\\\n]+))?$'
    )
    # _regex = (
    #     r'^\#define\s+(?P<name>\w+)(?P<macro_args>\('
    #     r'(?:\s*\w+\,\s*)*(?:\s*(?:(?:\w+)|(?:\.\.\.))\s*)?'
    #     r'\))?\s*'
    #     r'(?P<body>'
    #     r'(?:\s*[^\\\n]*\s*\\\n)*'
    #     r'(?:\s*[^\\\n]*))?$'
    # )
    _fstring_cond = (
        '#ifndef {name}\n'
        '#define {name}{macro_args} {body}\n'
        '#endif'
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
        '{name} {PREFIX[= ]:value},'
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
        'enum {name} {\n'
        '{members}\n'
        '};'
    )


class CTypeBaseUnit(CodeUnit):

    language = 'c'
    unit_type = 'base_type'
    _regex = (
        r'(?P<name>(?:(?:(?:(?:(?:un)?signed)|(?:long)|(?:short)) )+'
        r'(?:(?:int)|(?:double)|(?:char)|'
        r'(?:unsigned)|(?:long)|(?:short)))|'
        r'(?:(?!(?:return)|(?:operator)|'
        r'(?:RAPIDJSON\_DISABLEIF\_RETURN)|'
        r'(?:RAPIDJSON\_ENABLEIF\_RETURN))\w+))'
    )
    _fstring_cond = r'{name}'

    def as_type(self):
        return self.code_unit('type')(base=self)

    def utilized_type_units(self, *args, **kwargs):
        out = [self.as_type()]
        out += super(CTypeBaseUnit, self).utilized_type_units(*args, **kwargs)
        return out


class CTypeUnit(TypeUnit):

    _regex_fstring = (
        r'(?P<const>const\s+)?(?P<enum>enum\s+)?(?P<base>{NG:base_type})'
        r'(?:(?P<ptr_space1>\s+)?(?P<ptr>[\*]+))?'
    )
    _regex_nogroup_fstring = (
        r'(?:const\s+)?(?:enum\s+)?(?:{NG:base_type})'
        r'(?:\s*[\*]+)?'
    )
    _fstring_cond = (
        r'{const}{enum}{base}{ptr}'
    )
    _fstring_cond_shape = (
        r'{const}{enum}{base}{ptr}{shape}'
    )
    _properties = ['base']
    _properties_optional = [
        'const', 'enum', 'ptr', 'ptr_space1', 'item_ptr', 'shape'
    ]
    address_property = 'base'
    property_units = dict(
        TypeUnit.property_units,
        base='base_type',
    )

    def get_fstring(self, alt_fstring_cond=None, parent_result=None,
                    include_shape=None, **kwargs):
        if include_shape is None:
            include_shape = False
            if alt_fstring_cond is None and 'shape' in self.properties:
                include_shape = (parent_result is None)
                if parent_result:
                    def isvar(x):
                        if isinstance(x, list):
                            for xx in x:
                                if isvar(xx):
                                    return True
                        elif x.unit_type == 'variable':
                            return True
                        return False
                    for x in parent_result.solf_trace:
                        if isvar(x):
                            include_shape = True
                            break
        if include_shape:
            if parent_result:
                print(f"include shape in type with trace = "
                      f"{parent_result.solf_trace}?")
                import pdb
                pdb.set_trace()
            alt_fstring_cond = self._fstring_cond_shape
        return super(CTypeUnit, self).get_fstring(
            alt_fstring_cond=alt_fstring_cond,
            parent_result=parent_result, **kwargs)

    def replace_type(self, a, b, **kwargs):
        template_spec = self.properties.get('template_spec', None)
        if (((isinstance(a, CodeUnit)
              and self.properties['base'] == a.properties.get('base', '')
              and ((template_spec is None) or
                   template_spec == a.properties.get('template_spec', None)))
             or (isinstance(a, str)
                 and self.properties['base'].properties['name'] == a
                 and ((template_spec is None) or
                      template_spec == kwargs.get('template_spec', None))))):
            if isinstance(b, str):
                b = self.code_unit('type').parse(b)
            else:
                assert isinstance(b, CodeUnit)
            for k in ['base', 'template_spec', 'typename']:
                if k in b.properties:
                    self.set_property(k, b.properties[k])
        super(CTypeUnit, self).replace_type(a, b, **kwargs)


class CTypedefUnit(CodeUnit):

    language = 'c'
    unit_type = 'typedef'
    _properties = ['name', 'type']
    _regex_fstring = (
        r'^\s*typedef\s+(?P<type>{NG:type})\s+(?P<name>\w+)\s*;\s*'
    )
    _fstring_cond = 'typedef {type} {name};'

    def replace_type(self, a, b, **kwargs):
        new_name = do_replace_type_str(a, b, self.properties['name'],
                                       exact=True)
        if new_name is not None:
            self.set_property('name', new_name)
        super(CTypedefUnit, self).replace_type(a, b, **kwargs)


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
        new_docs = do_replace_type_str(self.properties['docs'])
        if new_docs is not None:
            self.set_property('docs', new_docs)
        super(CDocsUnit, self).replace_type(a, b, **kwargs)


class CInlineDocsUnit(CDocsUnit):

    unit_type = 'inline_docs'
    additional_languages = ['cxx']
    _regex = (
        r'(?P<prefix>\/)?(?:\/\*[\!\*])\<\s*'
        # r'(?P<docs>(?:.+?\s*(?:\n)?)+)' # \s*)+)'
        # r'(?P<docs>(?:(?:\s*\*\s*)?(?:[^\/]+\s*))+?)'
        r'(?P<docs>(?:[^\/])+?)'
        r'\s*(?(prefix)(?:)|(?:\*\/))'
    )
    _regex_nogroup = (
        r'(?:\/)?(?:\/\*[\!\*])\<\s*'
        # r'(?P<docs>(?:.+?\s*(?:\n)?)+)' # \s*)+)'
        # r'(?P<docs>(?:(?:\s*\*\s*)?(?:[^\/]+\s*))+?)'
        r'(?:(?:[^\/])+?)'
        r'\s*(?:\*\/)?'
    )
    _fstring_cond = '/**< {docs} */'


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

    def __init__(self, *args, **kwargs):
        super(CVariableUnit, self).__init__(*args, **kwargs)
        if self.properties.get('shape', False):
            self.properties['type'].properties['shape'] = (
                self.properties['shape'])


class CXXReturnMacro(CodeUnit):

    language = 'c'
    additional_languages = ['cxx']
    unit_type = 'return_macro'
    address_property = 'type'
    allow_duplicates = True
    no_forward_unit = True
    _properties = ['type']
    _properties_optional = []  # 'type', 'retmacro', 'cond']
    _regex_fstring = (
        # r'(?P<retmacro>RAPIDJSON\_(?:(?:DIS)|(?:EN))ABLEIF\_RETURN\('
        # r'\s*\((?P<cond>.+)\)\s*\,\s*\(\s*)?'
        r'(?P<type>{NG:type})'
        # r'(?(retmacro)(?:\s*\)\s*\)))' #?
    )


class CFunctionUnit(FunctionUnit):

    _regex_fstring = (
        # r'(?:\s*{docs}\s*)?'
        r'^(?P<indent>\s*)'
        r'{api}(?P<static>static\s+)?(?P<inline>inline\s+)?'
        r'(?P<type>{NG:type})\s+'
        # r'{return_macro}\s+'
        r'(?P<name>(?!operator)\w+)'
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
        '{IF[preprocess_contexts]:SUFFIX[\\n]:preprocess_contexts}'
        '{docs}'
        '{api}{static}{inline}{type} {name}({args}{va_args});'
        '{IF[preprocess_contexts]:PREFIX[\\n]:RITER[#endif]:'
        'preprocess_contexts}'
    )
    _properties = FunctionUnit._properties + [
        'type'
    ]
    _properties_optional = FunctionUnit._properties_optional + [
        'docs', 'api', 'static', 'inline', 'va_args', 'body',
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

    # def __init__(self, *args, **kwargs):
    #     super(CFunctionUnit, self).__init__(*args, **kwargs)
    #     if any('shape' in x.properties for x in self.properties['args']):
    #         fmts = ['{type:args}', '{args}']
    #         for fmt in fmts:
    #             print(f"FORMAT: {fmt} = {self.format(alt_fstring_cond=fmt)}")
    #         print("FINISHED")
    #         import pdb; pdb.set_trace()

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

    def check_duplicate(self, solf, macros, parent_match, parent_kws):
        for k in ['retmacro', 'argmacro']:
            if self.get_property(k, None) != solf.get_property(k, None):
                return False
        # ctx = self.get_property('preprocess_contexts', None,
        #                         parent_match=parent_match, **parent_kws)
        # print(ctx)
        return super(CFunctionUnit, self).check_duplicate(solf, macros,
                                                          parent_match,
                                                          parent_kws)

    def get_property(self, k, default=NoDefault, **kwargs):
        if ((k == 'preprocess_contexts' and k not in kwargs
             and k not in self.properties)):
            out = None
            tokens = None
            containers_parent = None
            containers = None
            ignored = []
            file_unit = self.get_property('file_unit', None)
            if file_unit is not None:
                tokens = file_unit.get_property('preprocess_tokens',
                                                default=None,
                                                **kwargs)
                preunit_file = file_unit.code_unit('preprocess_context')
                ignored = file_unit.get_property(
                    'ignored_preprocess_contexts', [])
            if tokens and self.match:
                containers_parent = tokens.tokens_containing(
                    self, include_tokens=['preprocess'])
                containers = [
                    x for x in containers_parent
                    if not x.contains(self.parent_unit)
                ]
                preunit_file = file_unit.code_unit('preprocess_context')
                out = [preunit_file.parse(x.group()) for x in containers]
                out = [
                    x for x in out if x.get_property('name') not in
                    ignored
                ]
            if out:
                self.set_property(k, out)
                return out
        kwargs['default'] = default
        return super(CFunctionUnit, self).get_property(k, **kwargs)


class CStructUnit(ClassUnit):

    unit_type = 'struct'
    member_context = ('{', '}')
    member_units = [
        'typedef', 'struct', 'enum', 'var',
    ]
    regex_fstring = (
        r'^(?P<indent>\s*)struct\s+(?P<name>\w+)\s*\{'
    )
    _fstring_cond = (
        'struct {name} {\n'
        '{members}\n'
        '};'
    )

    def replace_type(self, a, b, **kwargs):
        new_name = do_replace_type_str(a, b, self.properties['name'],
                                       exact=True)
        if new_name is not None:
            self.set_property('name', new_name)
        super(CStructUnit, self).replace_type(a, b, **kwargs)


class CImportUnit(ImportUnit):

    language = 'c'
    additional_languages = ['cxx']
    allow_duplicates = 'ignore'
    no_forward_unit = True
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
        '#include {IF[{local_ctx}]:STR[\"]:0}'
        '{IF[{global_ctx}]:STR[<]:0}{name}'
        '{IF[{global_ctx}]:STR[>]:0}'
        '{IF[{local_ctx}]:STR[\"]:0}'
    )

    def __init__(self, *args, **kwargs):
        if not (kwargs.get('local_ctx', False)
                or kwargs.get('global_ctx', False)):
            kwargs.setdefault('local_ctx', True)
        super(CImportUnit, self).__init__(*args, **kwargs)


class CFileUnit(FileUnit):
    r"""Class for generating/parsing C files."""

    ext = ['.h', '.c']
    comment = "//"
    block_comment = (["/*", "/**"], "*/")
    indent = '  '
    member_units = ['macro', 'function', 'typedef', 'import']
    context_tokens = {
        'preprocess': ContextToken(
            r"^\s*#if.*$", r"^\s*#endif.*$",
            name='preprocess', use_regex=True,
            subtokens=[r'^\s*(?:(?:#else)|(?:#elif).*$)']),
    }
    _properties_optional = FileUnit._properties_optional + [
        'includes', 'preprocess_tokens', 'ignored_preprocess_contexts',
    ]
    _fstring_cond = (
        '{includes}\n'
        '{members}'
    )
    property_subunits = dict(
        FileUnit.property_subunits,
        includes='import',
    )
    list_seps = dict(
        FileUnit.list_seps,
        includes='\n',
    )

    @classmethod
    def preprocess(cls, x, remove_tokens=None):
        x, kws = super(CFileUnit, cls).preprocess(x)
        skip_tokens = [token for token in cls.context_tokens.values() if
                       token.exclusive]
        if remove_tokens is None:
            remove_tokens = []
        remove_tokens = [cls.context_tokens['preprocess']] + remove_tokens
        out = x
        kws['recursive_member_kwargs'] = {}
        for token in remove_tokens:
            out, match = token.removeall(
                out, skip_tokens=skip_tokens, return_match=True)
            if match:
                kws['recursive_member_kwargs'][f"{token.name}_tokens"] = match
                match.preprocess_token = token
                kws[f"{token.name}_tokens"] = match
        # check_for=["#if", "#endif"],
        # check_adjusted=True, verbose=True)
        # if match:
        #     kws['recursive_member_kwargs'] = {
        #         'preprocess_tokens': match
        #     }
        #     kws['preprocess_tokens'] = match
        return out, kws


class CXXTemplateParamUnit(CodeUnit):

    language = 'cxx'
    unit_type = 'template_param'
    _properties = ['name', 'param_type']
    _properties_optional = CodeUnit._properties_optional + ['default']
    _regex_fstring = (
        r'(?P<param_type>(?:typename)|(?:\w+))'
        r'\s+(?P<name>\w+)(?:\s*\=\s*'
        r'(?P<default>(?:\w+)|(?:{NG:type})))?'
    )
    _fstring_cond = (
        r'{param_type} {name}{PREFIX[=]:default}'
    )

    def __init__(self, *args, **kwargs):
        super(CXXTemplateParamUnit, self).__init__(*args, **kwargs)
        is_typename = (self.properties['param_type'] == 'typename')
        for k in ['param_type', 'default']:
            v = self.properties.get(k, None)
            if ((v is None or isinstance(v, CodeUnit)
                 or (k == 'default' and (not is_typename))
                 or (k == 'param_type' and is_typename))):
                continue
            self.set_property(k, self.code_unit('type').parse(v))


class CXXForwardTemplateParamUnit(CXXTemplateParamUnit):

    unit_type = 'forward_template_param'
    _fstring_cond = (
        r'{param_type} {name}'
    )


class CXXTemplateUnit(CodeUnit):

    language = 'cxx'
    unit_type = 'template'
    _properties = []
    _properties_optional = ['param']
    _regex_fstring = (
        r'template\s*{RCONTAINER:param}'
    )
    _regex_nogroup = (
        r'template\s*\<\s*.+\s*\>'
    )
    _fstring_cond = (
        'template< {param} >\n'
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

    @classmethod
    def specialize_dict2list(cls, spec):
        if any(isinstance(v, list) for v in spec.values()):
            spec_values = [vv if isinstance(vv, list) else [vv]
                           for vv in spec.values()]
            additional_spec = [
                {k: v for k, v in zip(spec.keys(), x)}
                for x in itertools.product(*spec_values)
            ]
            return additional_spec
        return [spec]

    @classmethod
    def specialize_templated(cls, self, spec, param_types=None,
                             new_siblings=None, **kwargs):
        assert 'template' in self.properties
        name = self.address_local
        if param_types is None:
            param_types = {}
        template_siblings = []
        additional_spec = cls.specialize_dict2list(spec)
        spec = additional_spec[0]
        additional_spec = additional_spec[1:]
        if additional_spec:
            template_siblings = [type(self).from_unit(self)
                                 for _ in additional_spec]
            for x, xspec in zip(template_siblings, additional_spec):
                x.specialize(name, xspec, **kwargs)
        param = {
            x.properties['name']: x for x in
            self.properties['template'].properties['param']
        }
        template_spec = []
        for k, v in spec.items():
            assert not isinstance(v, list)
            if k not in param:
                raise AssertionError(
                    f"Parameter {k} missing for {k} = {v} "
                    f"template specialization\n"
                    f"{pprint.pformat(param)}")
            template_spec.append(v)
            if param[k].properties['param_type'] == 'typename':
                if isinstance(v, CodeUnit):
                    param_types[k] = v.format()
                else:
                    param_types[k] = v
            param.pop(k)
        self.properties['template'].properties['param'] = list(
            param.values())
        self.properties['specialized_template_spec'] = template_spec
        if param:
            for i, x in enumerate(
                    self.properties['template'].properties['param']):
                x.properties['subunit_index'] = i
        else:
            self.properties.pop('template')
        for k, v in param_types.items():
            self.replace_type(k, v)
        if kwargs.get('add_members_specialized', None):
            specT = tuple([v for v in spec.values()])
            if specT in kwargs['add_members_specialized']:
                self.add_members(
                    **kwargs['add_members_specialized'][specT])
        if kwargs.get('remove_members_specialized', None):
            specT = tuple([v for v in spec.values()])
            if specT in kwargs['remove_members_specialized']:
                self.remove_members(
                    **kwargs['remove_members_specialized'][specT])
        if template_siblings:
            if new_siblings is None:
                index = self.parent_unit.index(self.properties['name'])
                self.parent_unit.add_members(template_siblings,
                                             index=index)
            else:
                new_siblings[self.properties['name']] = template_siblings
        return template_spec


class CXXTypeBaseUnit(CTypeBaseUnit):

    language = 'cxx'
    unit_type = 'base_type'
    _regex = (
        r'((?P<base_classes>(?:\w+(?:\<.*?\>)?\:\:)*'
        r'\w+(?:\<.*?\>)?)\:\:)?'
        + CTypeBaseUnit._regex
    )
    _regex_norecurse = (
        r'(?:(?:\w+(?:\<.*?\>)?\:\:)*)'
        + CTypeBaseUnit._regex_norecurse
    )
    _fstring_cond = CTypeBaseUnit._fstring_cond.replace(
        r'{name}',
        r'{base_classes}{IF[{base_classes}]:STR[::]:0}{name}'
    )
    _properties_optional = CTypeUnit._properties_optional + [
        'base_classes',
    ]
    _ptr_replacements = [
        (r'[\*]', r'[\*\&]'),
        (r'[\*\s]', r'[\*\&\s]'),
    ]
    property_subunits = dict(
        CTypeBaseUnit.property_subunits,
        base_classes='type',
    )
    list_seps = dict(
        CTypeBaseUnit.list_seps,
        base_classes='::',
    )

    @classmethod
    def _replace_ptr(cls, out):
        for x, y in cls._ptr_replacements:
            out = out.replace(x, y)
        return out


class CXXTypeUnit(CTypeUnit):

    language = 'cxx'
    _regex_fstring = CXXTypeBaseUnit._replace_ptr(
        CTypeUnit._regex_fstring.replace(
            r'(?P<base>{NG:base_type})',
            r'(?P<typename>typename\s+)?'
            # (?P<base_module>(?:\w+\:\:)+)?'
            # r'(?(typename)(?P<base_classes>{NG:type}(\:\:{NG:type})*)\:\:)'
            r'(?P<base>{NG:base_type})'
            r'{CONTAINER:template_spec}')
    )
    _regex_norecurse_fstring = CTypeBaseUnit._regex.replace(
        r'\w+',
        r'(?:typename\s+)?(?:(?:\w+(?:\<.*?\>)?\:\:)*)\w+(?:\<.*?\>)?'
    )
    _regex_nogroup_fstring = None
    _fstring_cond = CTypeUnit._fstring_cond.replace(
        r'{base}',
        r'{typename}{base}{BOOKEND:template_spec}')
    _properties_optional = CTypeUnit._properties_optional + [
        'template_spec', 'typename',
    ]
    property_units = dict(
        CTypeUnit.property_units,
        base='base_type',
    )
    property_subunits = dict(
        CTypeUnit.property_subunits,
        template_spec='type',
    )


class CXXTypedefUnit(CTypedefUnit):

    language = 'cxx'


class CXXVariableUnit(CVariableUnit):

    language = 'cxx'
    _regex_fstring = CXXTypeBaseUnit._replace_ptr(
        CVariableUnit._regex_fstring
        + r'(?:\s*\=\s*(?P<rhs>[^,\)]+))?'
    )
    _regex_nogroup_fstring = CXXTypeBaseUnit._replace_ptr(
        CVariableUnit.get_regex(no_group=True,
                                return_fstring=True).replace(
            CTypeUnit.get_regex(no_group=True, return_fstring=True),
            CXXTypeUnit.get_regex(no_group=True, return_fstring=True))
        + r'(?:\s*\=\s*[^,\)]+)?'
    )
    _fstring_cond = (
        CVariableUnit._fstring_cond
        + r'{PREFIX[=]:rhs}'
    )
    _properties_optional = CVariableUnit._properties_optional + [
        'rhs', 'is_output',
    ]

    def __init__(self, *args, **kwargs):
        super(CXXVariableUnit, self).__init__(*args, **kwargs)
        if 'rhs' in self.properties:
            self.properties['rhs'] = self.properties['rhs'].rstrip(',')

    def replace_type(self, a, b, **kwargs):
        if 'rhs' in self.properties:
            new_rhs = do_replace_type_str(a, b, self.properties['rhs'])
            if new_rhs:
                self.properties['rhs'] = new_rhs
        super(CXXVariableUnit, self).replace_type(a, b, **kwargs)


class CXXFunctionUnit(CFunctionUnit):

    language = 'cxx'
    _regex_fstring = CFunctionUnit._regex_fstring.replace(
        r'^(?P<indent>\s*)',
        r'(?P<template>^\s*{template}\s*)?^(?P<indent>\s*)'
    )
    _fstring_cond = CFunctionUnit._fstring_cond.replace(
        '{api}',
        '{template}{api}'
    )
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
            r'(?P<static>static\s+)?',
            r'(?P<static>static\s+)?(?P<virtual>virtual\s+)?'
            r'(?P<friend>friend\s+(?:inline\s+)?)?')
    _fstring_cond = (
        '{IF[{new_access}]:NOINDENT}{new_access}{IF[{new_access}]:STR[:\n]}'
        + CXXFunctionUnit._fstring_cond.replace(
            ';',
            '{const_method}{override};').replace(
                '{api}',
                '{api}{virtual}{friend}')
    )
    _properties = CXXFunctionUnit._properties + [
        'parent'
    ]
    _properties_optional = CXXFunctionUnit._properties_optional + [
        'const_method', 'override', 'virtual', 'overloaded', 'access',
        'new_access', 'friend', 'specialized_template_spec',
        'wrapped_type', 'wrapped_template_spec', 'wrapped_type_conv',
    ]
    property_units = dict(
        CXXFunctionUnit.property_units,
        wrapped_type='type',
    )
    property_subunits = dict(
        CXXFunctionUnit.property_subunits,
        specialized_template_spec='type',
        wrapped_template_spec='type',
    )

    @classmethod
    def parse_property(cls, k, x):
        out = super(CXXMethodUnit, cls).parse_property(k, x)
        if k == 'va_args':
            out = ', ' + out
        return out

    def uses_method_type_spec(self, name):
        r"""
        Check if this method uses a specialized version of a class that
        is specific to this method's template parameters.

        Args:
            name (str): Name of the templated type to check for.

        Returns:
            bool, list: If the named type is specialized by the method
                template, return the template specialization parameters
                used from the template to specialize the type. Returns
                False otherwise.

        """
        template = self.properties.get('template', None)
        if not template:
            return False
        t_param = [x.properties['name'] for x in
                   template.properties['param']]
        for i, x in enumerate(self.properties['args']):
            x_type = x.properties['type']
            if x_type.properties['base'].properties['name'] == name:
                x_spec = [xx.format() for xx in
                          x_type.properties['template_spec']]
                if all(xx in t_param for xx in x_spec):
                    return x_spec
        return False

    def specialize(self, name, spec, **kwargs):
        template = self.properties.get('template', None)
        force = False
        if template and kwargs.get('specialize_method_args', False):
            method_spec = self.uses_method_type_spec(name)
            class_spec = kwargs.get('class_template_spec', None)
            if method_spec and class_spec:
                force = True
                spec = {k: v for k, v in zip(method_spec, class_spec)}
        if force or (name == self.address_local and template):
            CXXTemplateUnit.specialize_templated(
                self, spec, new_siblings=kwargs.get('new_siblings', None))
        super(CXXMethodUnit, self).specialize(name, spec, **kwargs)


class CXXOperatorUnit(CXXMethodUnit):

    unit_type = 'operator'
    _regex_fstring = CXXMethodUnit._regex_fstring.replace(
        r'(?P<type>{NG:type})',
        r'(?P<type>{NG:type})?'
    ).replace(
        r'(?P<name>(?!operator)\w+)',
        r'operator(?P<name>(?:[\(\)\[\]\+\-\=\\\*\%\<\>\w]+)|'
        r'(?P<cast_type>{NG:type}))\s*'
    )
    _fstring_cond = CXXMethodUnit._fstring_cond.replace(
        '{name}', 'operator{name}')
    _properties = [
        x for x in CXXMethodUnit._properties if x != 'type'
    ]
    _properties_optional = CXXMethodUnit._properties_optional + [
        'type', 'cast_type',
    ]
    property_units = dict(
        CXXMethodUnit.property_units,
        cast_type='type',
    )


class CXXConstructorUnit(CXXMethodUnit):

    unit_type = 'constructor'
    _regex_fstring = CXXMethodUnit._regex_fstring.replace(
        r'(?P<type>{NG:type})\s+',
        # r'{return_macro}\s+',
        r'(?P<explicit>explicit\s+)?'
    ).replace(
        r'(?P<name>(?!operator)\w+)', r'{KWS:parent}'
    ).replace(
        r'(?P<static>static\s+)?', ''
    ).replace(
        r'[;\{]', r'(?P<base_init>\:\s*[^;\{]+)?[;\{]'
    )
    _fstring_cond = CXXMethodUnit._fstring_cond.replace(
        '{type} ', '{explicit}'
    ).replace(
        '{static}', ''
    ).replace(
        '{friend}', ''
    ).replace(
        '{name}', '{parent}'
    ).replace(
        '{const_method}{override}', ''
    )
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
        '{api}{virtual}~{parent}();'
    )
    _properties = ['parent']
    _properties_optional = [
        'docs', 'api', 'virtual', 'body', 'access', 'new_acess',
    ]
    address_property = 'parent'


class CXXClassUnit(ClassUnit):

    member_units = [
        'constructor', 'destructor', 'method', 'operator', 'typedef',
        'class', 'enum',
    ]
    member_context = ('{', '}')
    members_discontiguous = True
    property_units = dict(
        ClassUnit.property_units,
        base_class='type',
    )
    _regex_fstring = (
        r'(?P<template>^\s*{template}\s*)?'
        r'^(?P<indent>\s*)(?P<meta>(?:class)|(?:struct))\s+(?P<name>\w+)'
        r'{CONTAINER:template_spec}(?:\s*\:\s*'
        r'(?P<base_scope>(?:public)|(?:private))\s+'
        r'(?P<base_class>{NG:type})'
        r')?'
        r'\s*\{'
    )
    _fstring_cond = (
        '{template}{meta} {name}{BOOKEND:template_spec}'
        '{IF[{base_class}]:STR[ : ]:0}{base_scope}'
        '{IF[{base_scope}]:STR[ ]:0}{base_class}'
        ' {\n'
        '{members}\n'
        '};'
    )
    _properties = ClassUnit._properties + [
        'meta',
    ]
    _properties_optional = [
        'macro', 'base_class', 'base_scope', 'base_class_unit',
        'template', 'template_spec', 'template_param',
        'specialized_type', 'specialized_template_spec',
        'instant_spec',
    ]
    property_units = dict(
        ClassUnit.property_units,
        base='type',
    )
    property_subunits = {
        'instant_spec': 'type',
        'template_spec': 'type',
        'specialized_template_spec': 'type',
    }
    _recursive_properties = ClassUnit._recursive_properties + [
        'specialized'
    ]

    def __init__(self, *args, **kwargs):
        kwargs.setdefault('meta', 'class')
        super(CXXClassUnit, self).__init__(*args, **kwargs)

    @property
    def address(self):
        out = super(CXXClassUnit, self).address
        if self.get_property('template_spec', None):
            out += (
                "<" + self.format_property(
                    'template_spec', self.get_property('template_spec'))
                + ">")
        return out

    def typedef_spec(self, name, spec):
        pass

    def specialized_types(self, spec):
        spec = CXXTemplateUnit.specialize_dict2list(spec)
        out = []
        for ispec in spec:
            template_spec = [
                ispec.get(x.properties['name'],
                          x.properties['name'])
                for x in self.properties['template'].properties['param']
            ]
            out.append(self.as_type(template_spec=template_spec))
        return out

    def as_type(self, add_module='', **kwargs):
        base_type_unit = self.code_unit('base_type')
        kwargs.setdefault(
            'base', base_type_unit.parse(
                add_module + self.get_property('fullname')))
        if 'template' in self.properties:
            # TODO: Handle non type params?
            type_unit = self.code_unit('type')
            kwargs.setdefault(
                'template_spec',
                [type_unit(base=base_type_unit.parse(
                    x.properties['name'])) for x in
                 self.properties['template'].properties['param']])
        return super(CXXClassUnit, self).as_type(**kwargs)

    def replace_type(self, a, b, **kwargs):
        new_name = do_replace_type_str(a, b, self.properties['name'],
                                       exact=True, use_name=True)
        if new_name is not None:
            self.set_property('name', new_name)
        super(CXXClassUnit, self).replace_type(a, b, **kwargs)

    def specialize(self, name, spec, new_name=None,
                   type_replacements=None, new_siblings=None, **kwargs):
        specialized = None
        if name == self.properties['name']:
            specialized = type(self).from_unit(self)
            template_spec = CXXTemplateUnit.specialize_templated(
                self, spec, type_replacements=type_replacements,
                new_siblings=new_siblings, new_name=new_name, **kwargs)
            if isinstance(new_name, dict):
                new_name = new_name[tuple(template_spec)]
            if new_name:
                old_name = self.properties['name']
                if new_siblings and old_name in new_siblings:
                    new_siblings[new_name] = new_siblings.pop(old_name)
                self.set_property('name', new_name)
                self.replace_type(old_name, new_name)
                assert type_replacements is not None
                specialized_type = specialized.as_type(
                    template_spec=template_spec)
                type_replacements[specialized_type] = new_name
                self.properties['specialized_type'] = specialized_type
            else:
                self.properties.setdefault('template_spec', [])
                self.properties['template_spec'] += template_spec
            kwargs['class_template_spec'] = template_spec
            kwargs['type_replacements'] = type_replacements
        super(CXXClassUnit, self).specialize(name, spec, **kwargs)
        # if specialized:
        #     self.properties['specialized'] = specialized

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
            if match.properties['access'] != previous_access:
                match.properties.setdefault(
                    'new_access', match.properties['access'])
            return match.properties['access']
        if not x:
            match.properties['access'] = 'public'
            if match.properties['access'] != previous_access:
                match.properties.setdefault(
                    'new_access', match.properties['access'])
            return match.properties['access']
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(x)
        pattern = re.compile(
            r'^\s*(?P<access>(?:public)|(?:protected)|(?:private))'
            r'\:\s*$',
            flags=re.MULTILINE)
        matches = list([
            m for m in pattern.finditer(x, pos, match.match_start)])
        if matches:
            match.properties['access'] = matches[-1].group('access')
        elif previous_access is None:
            match.properties['access'] = 'private'
        else:
            match.properties['access'] = previous_access
        if match.properties['access'] != previous_access:
            match.properties.setdefault(
                'new_access', match.properties['access'])
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


class CXXForwardClassUnit(CXXClassUnit):

    unit_type = 'forward_class'
    _regex_fstring = (
        r'(?P<template>^\s*{template}\s*)?'
        r'^(?P<indent>\s*)class\s+(?P<name>\w+);\s+'
    )
    _fstring_cond = (
        '{template}class {name};'
    )


class CXXModuleUnit(ModuleUnit):

    allow_duplicates = True
    member_units = [
        'macro', 'import', 'module', 'class', 'function', 'enum',
        'forward_class', 'typedef',
    ]
    member_context = ('{', '}')
    members_discontiguous = True
    _regex = (
        r'^(?P<indent>\s*)namespace\s+(?P<name>\w+)\s+\{'
    )
    _fstring_cond = (
        'namespace {name} {\n'
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
