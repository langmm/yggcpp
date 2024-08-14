import copy
from generate_generic.base import (
    create_mixin, NoDefault, CodeUnit, EnumValueUnit, EnumUnit,
    TypeUnit, VariableUnit, TypeConversionUnit, MethodUnit,
    ConstructorUnit, ClassUnit, ModuleUnit, FileUnit, FunctionUnit)
from generate_generic.cpp import (
    CXXTypeBaseUnit, CXXTypeUnit, CXXVariableUnit, CXXFunctionUnit,
    CXXMethodUnit, CXXModuleUnit, CXXFileUnit)


JuliaCXXWrapMixin = create_mixin('julia_cxxwrap')


class JuliaCXXWrapEnumValueUnit(JuliaCXXWrapMixin, EnumValueUnit):

    _fstring_cond = (
        '{indent}mod.set_const("{name}", {name:WR});'
    )


class JuliaCXXWrapEnumUnit(JuliaCXXWrapMixin, EnumUnit):

    _fstring_cond = (
        '{indent}mod.add_bits< enum {name:WR} >("{name}", '
        'jlcxx::julia_type("CppEnum"));\n'
        '{members}'
    )
    _fstring_cond_op = [
        'mod.method("|", [](const enum {name:WR}& a, const enum {name:WR}& b) '
        '{ return static_cast<{type:WR}>((a | b)); });',
        'mod.method("|", [](const enum {name:WR}& a, const {type:WR}& b) '
        '{ return static_cast<{type:WR}>((a | b)); });',
        'mod.method("|", [](const {type:WR}& a, const enum {name:WR}& b) '
        '{ return static_cast<{type:WR}>((a | b)); });',
    ]
    child_indent = 0

    def get_fstring(self, **kwargs):
        if self.properties.get('type', None):
            kwargs.setdefault(
                'alt_fstring_cond',
                self._fstring_cond + '\n{indent}'
                + '\n{indent}'.join(self._fstring_cond_op))
        return super(JuliaCXXWrapEnumUnit, self).get_fstring(**kwargs)


class JuliaCXXWrapTypeBaseUnit(JuliaCXXWrapMixin, CXXTypeBaseUnit):

    language = 'julia_cxxwrap'


class JuliaCXXWrapTypeUnit(JuliaCXXWrapMixin, CXXTypeUnit):

    language = 'julia_cxxwrap'
    _properties_optional = CXXTypeUnit._properties_optional + [
        'strictly_typed', 'wrapped_type',
    ]
    integer_types = [
        'int', 'unsigned', 'int64_t', 'uint64_t',
        'FLAG_TYPE', 'HEAD_FLAG_TYPE',
    ]
    # TODO: Parse this info from enums.hpp?
    enum_types = {
        'FLAG_TYPE': 'COMM_FLAG',
        'HEAD_FLAG_TYPE': 'HeadFlags',
    }
    enum_types_native = {
        'COMM_FLAG': 'int64_t',
        'HeadFlags': 'int',
    }

    def wrap_type(self):
        if ((self.properties['base'] in self.integer_types
             and not self.properties.get('ptr', False))):
            if not self.properties.get('strictly_typed', False):
                self.properties['strictly_typed'] = True
                self.properties['base'] = (
                    f"jlcxx::StrictlyTypedNumber<"
                    f"{self.properties['base']}"
                    f"{self.properties.get('ptr', '')}>")
                self.properties.pop('ptr', None)
            return True
        elif (self.properties['base'] == 'char'
              and self.properties.get('const', None)
              and self.properties.get('ptr', '*')):
            if not self.properties.get('wrapped_type', False):
                self.properties['wrapped_type'] = True
                self.properties['base'] = 'std::string'
                self.properties.pop('ptr', None)
            return True
        return False


class JuliaCXXWrapVariableUnit(JuliaCXXWrapMixin, CXXVariableUnit):

    language = 'julia_cxxwrap'

    def wrap_type(self):
        if self.properties['type'].wrap_type():
            return True
        return False

    @property
    def wrapped_arg(self):
        if self.properties['type'].properties.get('strictly_typed', False):
            return self.properties['name'] + '.value'
        elif self.properties['type'].properties.get('wrapped_type', False):
            return self.properties['name'] + '.c_str()'
        return self.properties['name']


class JuliaCXXWrapFunctionUnit(JuliaCXXWrapMixin, CXXFunctionUnit):

    _fstring_cond = (
        '{SUFFIX[\\n]:preprocess_contexts:BS:WR}'
        '{indent}mod.method'
        '< {type:WR}{CPREFIX[, ]:type:args:C:0} >'
        '("{name}", &{name:WR});'
        '{PREFIX[\\n]:RITER[#endif]:preprocess_contexts:BS:WR}'
    )
    _fstring_cond_body = (
        '{SUFFIX[\\n]:preprocess_contexts:BS:WR}'
        '{indent}mod.method'
        '("{name}", []({args}) '
        '{ {wrapper_body} });'
        '{PREFIX[\\n]:RITER[#endif]:preprocess_contexts:BS:WR}'
    )

    def get_fstring(self, **kwargs):
        if 'wrapper_body' in self.properties:
            kwargs.setdefault(
                'alt_fstring_cond', self._fstring_cond_body)
        return super(JuliaCXXWrapFunctionUnit, self).get_fstring(
            **kwargs)


class JuliaCXXWrapMethodUnit(JuliaCXXWrapMixin, CXXMethodUnit):

    _properties_optional = CXXMethodUnit._properties_optional + [
        'wrapper_body'
    ]
    _fstring_cond = (
        '{SUFFIX[\\n]:preprocess_contexts:BS:WR}'
        '{indent}{wrapper_inst:PR}'
        '{IF[{static:BS:WR}]:STR[.module()]:0}.method'
        '< {type:WR}{IFNOT[{static:BS:WR}]:PREFIX[, ]:parent:BS:WR}'
        '{CPREFIX[, ]:type:args:C:0} >'
        '("{name}", &{parent:BS:WR}'
        '{CBOOKEND[< ][ >]:template_spec:PR:BS:WR}'
        '::{name:WR});'
        '{PREFIX[\\n]:RITER[#endif]:preprocess_contexts:BS:WR}'
    )
    _fstring_cond_body = (
        '{SUFFIX[\\n]:preprocess_contexts:BS:WR}'
        '{indent}{wrapper_inst:PR}'
        '{IF[{static:BS:WR}]:STR[.module()]:0}.method'
        '("{name}", []('
        '{IFNOT[{static:BS:WR}]:SUB[{const_method:BS:WR} '
        '{parent:BS:WR}& self{IF[{args}]:STR[, ]:0}]:0}'
        '{args}) '
        '{ {wrapper_body} });'
        '{PREFIX[\\n]:RITER[#endif]:preprocess_contexts:BS:WR}'
    )

    def __init__(self, *args, **kwargs):
        super(JuliaCXXWrapMethodUnit, self).__init__(*args, **kwargs)
        JuliaCXXWrapMethodUnit.handle_wrapped_types(
            self, f"self.{self.wrapped_unit.properties['name']}")

    def get_fstring(self, **kwargs):
        if 'wrapper_body' in self.properties:
            kwargs.setdefault(
                'alt_fstring_cond', self._fstring_cond_body)
        try:
            return super(JuliaCXXWrapMethodUnit, self).get_fstring(
                **kwargs)
        except KeyError:
            print(kwargs.keys())
            print(self.wrapped_unit.properties.keys())
            import pdb
            pdb.set_trace()
            raise

    @staticmethod
    def handle_wrapped_types(self, name):
        for x in self.properties['args']:
            if x.wrap_type():
                self.properties['wrapped_type'] = True
        # Defaults
        core_args = []
        deft_args = []
        defaults = []
        for x in self.properties['args']:
            if deft_args or x.properties.get('rhs', None):
                deft_args.append(x)
                defaults.append(x.properties.pop('rhs'))
            else:
                core_args.append(x)
        self.properties['args'] = core_args
        if deft_args:
            members_for_parent = []
            for i in range(len(deft_args)):
                new_version = copy.deepcopy(self)
                new_version.properties['args'] = copy.deepcopy(
                    core_args + deft_args[:(i + 1)])
                new_version.properties['defaults'] = copy.deepcopy(
                    defaults[(i + 1):])
                JuliaCXXWrapMethodUnit.set_wrapper_body(new_version, name,
                                                        force=True)
                members_for_parent.append(new_version)
            self.append_properties_for_parent(
                {'members': members_for_parent})
            self.properties['defaults'] = defaults
        # Strict types
        JuliaCXXWrapMethodUnit.set_wrapper_body(self, name)

    @staticmethod
    def set_wrapper_body(self, name, force=False):
        if ((force or self.properties.get('defaults', False)
             or self.properties.get('wrapped_type', False))):
            wargs = ', '.join(
                ([x.wrapped_arg for x in self.properties['args']]
                 + self.properties.get('defaults', [])))
            ret = ''
            if ((self.unit_type == 'constructor'
                 or (self.properties['type'].properties['base']
                     not in ['void', 'GenericValue']))):
                ret = 'return '
            self.properties['wrapper_body'] = f"{ret}{name}({wargs});"


class JuliaCXXWrapConstructorUnit(JuliaCXXWrapMixin, ConstructorUnit):

    _fstring_cond = (
        '{indent}{wrapper_inst:PR}.constructor< {type:args:WR} >();'
    )
    _fstring_cond_method = (
        '{indent}{wrapper_inst:PR}.method("_construct", []({args})'
        '{ {wrapper_body} });'
    )
    _properties_optional = ConstructorUnit._properties_optional + [
        'wrapper_body'
    ]

    def __init__(self, *args, **kwargs):
        super(JuliaCXXWrapConstructorUnit, self).__init__(*args, **kwargs)
        JuliaCXXWrapMethodUnit.handle_wrapped_types(
            self, self.wrapped_unit.parent_unit.properties['name'])

    def get_fstring(self, **kwargs):
        if 'wrapper_body' in self.properties:
            kwargs.setdefault(
                'alt_fstring_cond', self._fstring_cond_method)
        return super(JuliaCXXWrapConstructorUnit, self).get_fstring(
            **kwargs)


class JuliaCXXWrapClassUnit(JuliaCXXWrapMixin, ClassUnit):

    _fstring_cond = (
        '{indent}auto {wrapper_inst} = mod.add_type< {name:WR}'
        '{CBOOKEND[< ][ >]:template_spec:WR} >'
        '("{name}"'
        '{BOOKEND[, jlcxx::julia_base_type< ][ >()]:base_class:WR}'
        ');\n'
        '{members}'
    )
    _fstring_cond_template = (
        '{indent}auto {wrapper_inst} = mod.add_type< Parametric< '
        '{JOIN[, ]:ITER[TypeVar< {XXX} >]:ADD[1]:member_index:param:'
        'template:WR}'
        ' > >("{name}"'
        '{BOOKEND[, wrapped_][.dt()]:base_class}'
        ');\n'
        '{indent}{wrapper_inst}.apply< '
        '{JOIN[, ]:ITER[{name:WR}< {XXX} >]:template_spec:WR}'
        ' >({wrapper_class}());'
    )
    dont_cache = ['regex']
    _properties_optional = ClassUnit._properties_optional + [
        'wrapper_inst', 'wrapper_class',
    ]
    child_indent = 0

    def get_property(self, k, default=NoDefault, **kwargs):
        kwargs['default'] = default
        if k not in kwargs:
            if k == 'wrapper_inst':
                return 'wrapped_' + self.get_property('name', **kwargs)
            elif k == 'wrapper_class':
                return 'Wrapped' + self.get_property('name', **kwargs)
        return super(JuliaCXXWrapClassUnit, self).get_property(k, **kwargs)

    def get_fstring(self, **kwargs):
        if ((('template' in self.wrapped_unit.properties)
             and (not self.wrapped_unit.properties.get(
                 'template_spec', '')))):
            kwargs.setdefault(
                'alt_fstring_cond', self._fstring_cond_template)
        try:
            return super(JuliaCXXWrapClassUnit, self).get_fstring(**kwargs)
        except KeyError:
            print(kwargs.keys())
            print(self.wrapped_unit.properties.keys())
            import pdb
            pdb.set_trace()
            raise


class JuliaCXXWrapSuperType(JuliaCXXWrapMixin, CodeUnit):

    unit_type = 'super_type_class'
    _fstring_cond = (
        '{indent}template<> struct SuperType< {fullname:BS:WR:WR}'
        '{CBOOKEND[< ][ >]:template_spec:C:BS:WR:WR} > \n'
        '{indent}{ typedef {fullname:BC:BS:WR:WR}'
        '{CBOOKEND[< ][ >]:template_spec:C:BC:BS:WR:WR} type; };'
    )
    _properties = [
        'name',
    ]
    list_seps = {
        'template_spec': ', ',
    }


class JuliaCXXWrapTemplateWrapperMethod(JuliaCXXWrapMixin, CodeUnit):

    unit_type = 'template_wrapper_method'
    _fstring_cond = (
        '{indent}{PR:wrapper_inst}'
        '{IF[{static:BS:WR}]:STR[.module()]:0}.method'
        '< {type:WR}, WrappedT{CPREFIX[, ]:type:args:WR} >'
        '("{name}", &WrappedT::{name});'
    )
    _properties = [
        'name'
    ]


class JuliaCXXWrapTemplateWrapperConstructor(JuliaCXXWrapMixin, CodeUnit):

    unit_type = 'template_wrapper_constructor'
    _fstring_cond = (
        '{indent}{wrapper_inst:PR}.constructor'
        '{IF[{args:WR}]:SUB[< {type:args:WR} >]:0}();'
    )
    _properties = [
        'parent'
    ]


class JuliaCXXWrapTemplateWrapper(JuliaCXXWrapMixin, CodeUnit):

    unit_type = 'template_wrapper_class'
    _fstring_cond = (
        '{indent}struct {wrapper_class:WR} {\n'
        '{indent}  template<typename TypeWrapperT>\n'
        '{indent}  void operator()(TypeWrapperT&& wrapped)\n'
        '{indent}  {\n'
        '{indent}    typedef typename TypeWrapperT::type WrappedT;\n'
        '{indent}    {members}\n'
        '{indent}  }\n'
        '{indent}};'
    )
    _properties = [
        'name'
    ]
    _properties_optional = [
        'members',
    ]
    member_units = [
        'template_wrapper_method',
        'template_wrapper_constructor',
    ]
    child_indent = 2


class JuliaCXXWrapPreamble(JuliaCXXWrapMixin, CodeUnit):

    unit_type = 'preamble'
    member_units = ['template_wrapper_class']
    _fstring_cond = (
        '{UNIQUE:ITER[#include \"{RELPATHC:XXX}\"]:includes}\n'
        '{members}\n'
    )
    _properties = []
    _properties_optional = [
        'includes',
        'members',
    ]
    list_seps = dict(
        CodeUnit.list_seps,
        includes='\n',
    )
    child_indent = 0


class JuliaCXXWrapJLCXXMod(JuliaCXXWrapMixin, CXXModuleUnit):

    unit_type = 'jlcxx_mod'
    member_units = ['super_type_class']
    _properties = ['name']
    _properties_optional = [
        'members'
    ]

    def __init__(self, *args, **kwargs):
        kwargs['name'] = 'jlcxx'
        super(JuliaCXXWrapJLCXXMod, self).__init__(*args, **kwargs)


class JuliaCXXWrapModuleUnit(JuliaCXXWrapMixin, ModuleUnit):

    member_units = ['class', 'function', 'enum']
    _fstring_cond = (
        'JLCXX_MODULE define_module_{name}(jlcxx::Module& mod)\n'
        '{\n'
        '  using namespace {fullname:BS:WR};\n'
        '{members}\n'
        '}'
    )


class JuliaCXXWrapFileUnit(JuliaCXXWrapMixin, CXXFileUnit):

    language = 'julia_cxxwrap'
    divider_char = '#'
    member_units = ['module', 'jlcxx_mod']
    ignored_units = [
        'macro', 'destructor', 'operator',
        'template', 'template_param',
        'super_type_method', 'preprocess_context',
    ]
    _fstring_cond = (
        '#include "jlcxx/jlcxx.hpp"\n'
        '#include "{RELPATHC:name:WR}"\n'
        '{preamble}\n'
        '{jlcxx_mod}\n\n'
        '{members}\n'
    )
    wrapper_type = 'jl_value_t*'
    _properties_optional = CXXFileUnit._properties_optional + [
        'preamble', 'jlcxx_mod'
    ]
    _properties_defaults_units = dict(
        CXXFileUnit._properties_defaults_units,
        preamble=JuliaCXXWrapPreamble,
        jlcxx_mod=JuliaCXXWrapJLCXXMod,
    )

    def __init__(self, *args, **kwargs):
        super(JuliaCXXWrapFileUnit, self).__init__(*args, **kwargs)
        self.instrument_classes()

    def instrument_classes(self):
        for x in self.iter_child_members():
            if x.unit_type != 'class' or not x.wrapped_unit:
                continue
            if ((x.wrapped_unit.properties.get('template', '')
                 and not x.wrapped_unit.properties.get(
                     'template_spec', ''))):
                self.properties['preamble'].add_member(
                    JuliaCXXWrapTemplateWrapper.from_unit(
                        x, conversion_prefix='template_wrapper_'))
            elif x.wrapped_unit.get_property('base_class', ''):
                self.properties['jlcxx_mod'].add_member(
                    JuliaCXXWrapSuperType.from_unit(
                        x, conversion_prefix='super_type_'))


class JuliaTypeUnit(TypeUnit):

    typemap = {
        'std::basic_string': 'String',
        'std::string': 'String',
        'char*': 'Cstring',
        'double': 'Float64',
        'float': 'Float32',
        'bool': 'Core.Bool',
        'int': 'CxxLong',
        'long': 'CxxLong',
        'unsigned': 'CxxULong',
        'unsigned int': 'CxxULong',
        'unsigned long': 'CxxULong',
        'int8_t': 'Core.Int8',
        'uint8_t': 'Core.UInt8',
        'int16_t': 'Core.Int16',
        'uint16_t': 'Core.UInt16',
        'int32_t': 'Core.Int32',
        'uint32_t': 'Core.UInt32',
        'int64_t': 'Core.Int64',
        'uint64_t': 'Core.UInt64',
        'rapidjson::Document': 'Document',
        'rapidjson::Value': 'Value',
    }
    typemap_cxx = {
        'std::basic_string': 'StdString',
        'std::string': 'StdString',
        'bool': 'CxxBool',
    }
    default_flags = {
        'COMM_FLAG': ['COMM_FLAG_INTERFACE'],
    }
    rapidjson_types = ['Document', 'Value']
    typemap_rj = {  # Order determines order of type checking
        'String': 'std::string',
        'Int64': 'int64_t',
        'Uint64': 'uint64_t',
        'Int': 'int',
        'Uint': 'unsigned',
        'Float': 'float',
        'Double': 'double',
    }
    address_property = 'full'
    _properties = ['full', 'base', 'orig', 'native']
    _properties_optional = ['enum', 'base_cxx', 'full_cxx']
    _fstring_cond = (
        '{full}'
    )

    def __init__(self, *args, **kwargs):
        orig = kwargs['base'] + kwargs.get('ptr', '').replace('&', '')
        native = orig
        if orig in JuliaCXXWrapTypeUnit.enum_types:
            kwargs['enum'] = JuliaCXXWrapTypeUnit.enum_types[orig]
            native = JuliaCXXWrapTypeUnit.enum_types_native[
                kwargs['enum']]
        base = self.typemap.get(orig, self.typemap.get(native, native))
        base_cxx = self.typemap_cxx.get(
            orig, self.typemap_cxx.get(native, base))
        full = copy.deepcopy(base)
        if kwargs.get('enum', None):
            if not isinstance(full, list):
                full = [full]
            full.append(kwargs['enum'])
        if base_cxx != full:
            full_cxx = copy.deepcopy(base_cxx)
            if isinstance(full_cxx, list):
                full_cxx = f"Union{{{', '.join(full_cxx)}}}"
            kwargs.update(base_cxx=base_cxx, full_cxx=full_cxx)
        if isinstance(full, list):
            full = f"Union{{{', '.join(full)}}}"
        if isinstance(base, list):
            base = base[0]
        kwargs.update(orig=orig, base=base, full=full, native=native)
        super(JuliaTypeUnit, self).__init__(*args, **kwargs)
        self.add_required_conversions()

    def converted_arg(self, x):
        if not self.properties.get('base_cxx', False):
            return x
        base_cxx = self.properties['base_cxx']
        if isinstance(base_cxx, list):
            base_cxx = base_cxx[0]
        out = f"convert({base_cxx}, {x})"
        if self.properties.get('enum', None) in self.default_flags:
            default_flags = ' | '.join(
                self.default_flags[self.properties['enum']])
            out = f"({out} | convert({base_cxx}, {default_flags}))"
        return out

    def add_required_conversions(self):
        converters = []
        file_converters = []
        if self.properties.get('enum', None):
            src = self.properties['enum']
            dsts = self.properties.get('base_cxx', self.properties['base'])
            if not isinstance(dsts, list):
                dsts = [dsts]
            for dst in dsts:
                kws = {'source_type': src, 'type': dst}
                converters.append(JuliaTypeConversionUnit(**kws))
        elif self.properties['base'] in self.rapidjson_types:
            body = []
            for k, v in self.typemap_rj.items():
                dst = self.typemap[v]
                kws = {'source_type': self.properties['base'],
                       'type': dst, 'rj_type': k, 'promote': True}
                file_converters.append(JuliaTypeConversionUnit(**kws))
                body += [
                    f'if (Is{k}(x))',
                    f'  return convert({dst}, x)',
                    'end',
                ]
            kws = {'body': body + ['return x'],
                   'source_type': self.properties['base'],
                   'type': self.properties['base']}
            file_converters.append(JuliaTypeConversionUnit(**kws))
        if converters:
            self.append_properties_for_parent(
                {('members', 0): converters,
                 'base_imports': ['convert', 'promote_rule']},
                key='module')
        if file_converters:
            self.append_properties_for_parent(
                {'members': file_converters,
                 'base_imports': ['convert', 'promote_rule']},
                key=('module', 'rapidjson'))


class JuliaVariableUnit(VariableUnit):

    _fstring_cond = (
        '{name}::{type}{IF[{rhs}]:STR[=]:0}{rhs}'
    )
    _properties = ['name', 'type']
    _properties_optional = VariableUnit._properties_optional + [
        'rhs', 'is_output',
    ]


class JuliaConstructorUnit(ConstructorUnit):

    _fstring_cond = (
        '{indent}{name:PR:GN}({args}{IF[{kwargs}]:STR[; ]:0}'
        '{kwargs}) = '
        '_construct({JOIN[, ]:convert_args})'
    )
    _properties = ConstructorUnit._properties + [
        'convert_args',
    ]
    _properties_optional = ConstructorUnit._properties_optional + [
        'kwargs',
    ]
    property_subunits = dict(
        ConstructorUnit.property_subunits,
        kwargs='var',
    )
    list_seps = dict(
        ConstructorUnit.list_seps,
        kwargs=', ',
    )

    def __init__(self, *args, **kwargs):
        args0 = kwargs.get('args', [])
        convert_args0 = kwargs.get('convert_args', [])
        nargs0 = len(args0)
        if len(convert_args0) != nargs0:
            assert not convert_args0
            for x in args0:
                convert_args0.append(
                    x.properties['type'].converted_arg(
                        x.properties['name']))
        if kwargs.get('generating_unit', None):
            nargs0 = len(kwargs['generating_unit'].properties['args'])
        kwargs.update(args=args0[:nargs0],
                      convert_args=convert_args0,
                      kwargs=args0[nargs0:])
        super(JuliaConstructorUnit, self).__init__(*args, **kwargs)

    def get_fstring(self, **kwargs):
        if (((not self.generating_unit)
             or ('wrapper_body' not in self.generating_unit.properties))):
            return ''
        out = super(JuliaConstructorUnit, self).get_fstring(**kwargs)
        return out


class JuliaTypeConversionUnit(TypeConversionUnit):

    _properties = ['type', 'source_type']
    _properties_optional = TypeConversionUnit._properties_optional + [
        'rj_type', 'body', 'promote',
    ]
    address_property = 'type'
    _fstring_cond = (
        '{indent}convert(::Type{ {type} }, x::{source_type}) = '
        'reinterpret({type}, x)'
    )
    _fstring_cond_rj = (
        '{indent}function convert(::Type{ {IF[{promote}]:STR[<:]:0} '
        '{type} }, x::{source_type})\n'
        '{indent}  if (!Is{rj_type}(x))\n'
        '{indent}    throw("{source_type} is not a {rj_type}")\n'
        '{indent}  end\n'
        '{indent}  return convert({type}, Get{rj_type}(x))\n'
        '{indent}end\n'
        '{indent}function {source_type}(y::{type})\n'
        '{indent}  x = {source_type}()\n'
        '{indent}  Set{rj_type}(x, y)\n'
        '{indent}  return x\n'
        '{indent}end\n'
        '{indent}function Set{source_type}(x::{source_type}, y::{type})\n'
        '{indent}  Set{rj_type}(x, y)\n'
        '{indent}end'
    )
    _fstring_cond_extract = (
        '{indent}function extract(x::{source_type})\n'
        '{indent}  {JOIN[\n{indent}  ]:body}\n'
        '{indent}end'
    )
    _fstring_promote_rj = (
        '{indent}promote_rule(::Type{ {type} }, '
        '::Type{ <:{source_type} })'
    )

    def get_fstring(self, **kwargs):
        set_alt = ('alt_fstring_cond' not in kwargs)
        if self.properties.get('body', None):
            kwargs.setdefault(
                'alt_fstring_cond', self._fstring_cond_extract)
        elif self.properties.get('rj_type', None):
            kwargs.setdefault(
                'alt_fstring_cond', self._fstring_cond_rj)
        if kwargs.get('promote', False) and not set_alt:
            kwargs['alt_fstring_cond'] + self._fstring_promote_rj
        return super(JuliaTypeConversionUnit, self).get_fstring(**kwargs)


class JuliaEnumUnit(EnumUnit):

    _fstring_cond = (
        '{indent}|(a::{name}, b::{name}) = '
        '(reinterpret({base:type}, a) | reinterpret({base:type}, b))'
    )

    def __init__(self, *args, **kwargs):
        super(JuliaEnumUnit, self).__init__(*args, **kwargs)
        self.append_properties_for_parent(
            {'base_imports': ['|']}, key='module')

    def get_fstring(self, **kwargs):
        if not self.properties.get('type', None):
            return ''
        return super(JuliaEnumUnit, self).get_fstring(**kwargs)


class JuliaFunctionUnit(FunctionUnit):

    _fstring = ''

    def __init__(self, *args, **kwargs):
        super(JuliaFunctionUnit, self).__init__(*args, **kwargs)
        if self.properties.get('call_at_exit', False):
            self.append_properties_for_parent(
                {'atexit': [self.properties['name']]}, key='module')

    def get_fstring(self, **kwargs):
        return ''


class JuliaMethodUnit(MethodUnit):

    _fstring_cond = (
        '{indent}function {name}('
        'self::Union{ {JOIN[, ]:name:GN:CC:PR} }, {input_args})\n'
        '{indent}  {name:var} = {type:var}()\n'
        '{indent}  flag = {name}(self, '
        '{JOIN[, ]:name:input_args_front}'
        '{IF[{input_args_front}]:STR[, ]:0}'
        'CxxRef({name:var})'
        '{IF[{input_args_back}]:STR[, ]:0}'
        '{JOIN[, ]:name:input_args_back}'
        ')\n'
        '{indent}  return flag, '
        '{IF[{extract_var}]:STR[extract(]:0}{name:var}'
        '{IF[{extract_var}]:STR[)]:0}\n'
        '{indent}end'
    )

    _properties_optional = MethodUnit._properties_optional + [
        'input_args', 'input_args_front', 'input_args_back', 'var',
        'extract_var',
    ]
    property_subunits = dict(
        MethodUnit.property_subunits,
        input_args='var',
        input_args_front='var',
        input_args_back='var',
    )
    list_seps = dict(
        MethodUnit.list_seps,
        input_args=', ',
        input_args_front=', ',
        input_args_back=', ',
    )

    def __init__(self, *args, **kwargs):
        input_args_front = []
        input_args_back = []
        output_args = []
        for i, x in enumerate(kwargs['args']):
            if x.properties.get('is_output', False):
                output_args.append(x)
                x.properties['is_output'] = i
            elif output_args:
                input_args_back.append(x)
            else:
                input_args_front.append(x)
        if output_args:
            assert len(output_args) == 1
            kwargs.update(input_args_front=input_args_front,
                          input_args_back=input_args_back,
                          input_args=(input_args_front + input_args_back),
                          var=output_args[0])
            if ((output_args[0].properties['type'].properties['base']
                 in JuliaTypeUnit.rapidjson_types)):
                kwargs['extract_var'] = True
        super(JuliaMethodUnit, self).__init__(*args, **kwargs)
        if output_args:
            solf = copy.deepcopy(self)
            solf.properties['top_level'] = self
            self.append_properties_for_parent(
                {'exports': self.properties['name']}, key='module')
            self.append_properties_for_parent(
                {'members': [solf]}, key='file')

    def get_fstring(self, **kwargs):
        if not self.properties.get('var', None):
            return ''
        top_level = self.properties.get('top_level', False)
        parent_module = (top_level.find_parent(member_units=['module'])
                         if top_level else
                         self.find_parent(member_units=['module']))
        uses_parent_type = any(
            x in parent_module.properties.get('external_types', [])
            for x in self.utilized_types())
        if (((top_level and (not uses_parent_type))
             or ((not top_level) and uses_parent_type))):
            return ''
        if top_level:
            return super(JuliaMethodUnit, top_level).get_fstring(**kwargs)
        return super(JuliaMethodUnit, self).get_fstring(**kwargs)


class JuliaClassUnit(ClassUnit):

    _fstring_cond = '{members}'
    member_units = ['constructor', 'method']
    child_indent = 0

    def __init__(self, *args, **kwargs):
        super(JuliaClassUnit, self).__init__(*args, **kwargs)
        if self.properties.get('type_constructors', False):
            members_for_parent = []
            # TODO: Add converters based on constructors
            if members_for_parent:
                self.append_properties_for_parent(
                    {'members': members_for_parent}, key='module')


class JuliaModuleUnit(ModuleUnit):

    _properties = [
        'name', 'parent',
    ]
    _properties_optional = ModuleUnit._properties_optional + [
        'members', 'exports', 'base_imports', 'atexit',
    ]
    member_units = ['class', 'function', 'type_conversion', 'enum']
    _fstring_cond = (
        'module lib{name}\n'
        '  using CxxWrap\n'
        '  {IF[{base_imports}]:STR[import Base: ]:0}{UNIQUE:base_imports}\n'
        '  @wrapmodule(() -> "{LIBFILE:parent:GN}",:define_module_{name})\n'
        '  function __init__()\n'
        '    @initcxx\n'
        '  end\n'
        '{members}\n'
        '  export {JOIN[, ]:name:members:generating_unit}'
        '{IF[{exports}]:STR[, ]:0}{exports}\n'
        '  {JOIN[\n  ]:ITER[atexit( {XXX} )]:atexit}\n'
        'end\n'
        'using .lib{name}\n'
        'for name in names(lib{name}; all=true)\n'
        '  (name in exclude || !isdefined(lib{name}, name)) && continue\n'
        '  startswith(string(name), "#") && continue\n'
        '  startswith(string(name), "__cxxwrap") && continue\n'
        '  @eval import .lib{name}: $name\n'
        '  @eval export $name\n'
        'end'
    )
    list_seps = dict(
        ModuleUnit.list_seps,
        exports=', ',
        base_imports=', ',
        atexit=', ',
    )


class JuliaFileUnit(FileUnit):

    ext = ['.jl']
    comment = "#"
    divider_char = '#'
    indent = 2 * ' '
    ignored_units = [
        'macro', 'destructor', 'enum_value', 'operator',
        'template', 'template_param', 'preprocess_context',
    ]
    language_wrapped = 'cxx'
    generating_unit_type = 'julia_cxxwrap'
    _fstring_cond = (
        'module YggInterface\n'
        'using CxxWrap\n'
        'const exclude = [:__init__, :eval, :include]\n'
        '{members}\n'
        'end'
    )
    child_indent = 0
