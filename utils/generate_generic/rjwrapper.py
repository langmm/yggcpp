import copy
from generate_generic.base import (
    CodeUnit, CodeUnitMeta, code_unit_registry)


def create_rj_wrapper_source(cls):

    fstring_cond_src = cls._fstring_cond_src
    if fstring_cond_src is None:
        fstring_cond_src = ''
        if hasattr(cls, '_cxxcls'):
            fstring_cond_src = cls._cxxcls._fstring_cond
            if fstring_cond_src is not None:
                for k, v in getattr(cls, '_src_repl', []):
                    fstring_cond_src = fstring_cond_src.replace(k, v)
                fstring_cond_src = (
                    getattr(cls, '_src_prefix', '')
                    + fstring_cond_src
                    + getattr(cls, '_src_suffix', '')
                )
    fstring_cond_prop = getattr(cls, '_src_fstring_cond_prop', {})
    if hasattr(cls, '_cxxcls'):
        fstring_cond_prop = dict(cls._cxxcls._fstring_cond_prop,
                                 **fstring_cond_prop)

    class RJWrapperSource(cls):

        language = 'rjwrapper_source'
        _fstring_cond = fstring_cond_src
        _fstring_cond_prop = fstring_cond_prop
        _header_class = cls

    return RJWrapperSource


class RJWrapperMixinMeta(CodeUnitMeta):

    def __new__(meta, name, bases, class_dict):
        class_dict.setdefault('language', 'rjwrapper')
        skip_registry_temp = class_dict.get('skip_registry_temp', False)
        if class_dict['language'] == 'rjwrapper_source':
            name = class_dict['_header_class'].__name__.replace(
                "Unit", "SourceUnit")
            class_dict['__qualname__'] = name
        cls = CodeUnitMeta.__new__(meta, name, bases, class_dict)
        if cls.language != 'rjwrapper_source' and not skip_registry_temp:
            cls._source_class = create_rj_wrapper_source(cls)
        return cls


_cond_src = {}


def RJWrapperCXX(unit_type, cond_src=None, full_class=False):
    cxxcls = code_unit_registry(language='cxx')[unit_type]
    if cond_src is None:
        cond_src = _cond_src.get(unit_type, None)

    class RJWrapperCXXUnit(cxxcls, metaclass=RJWrapperMixinMeta):

        language = 'rjwrapper'
        _fstring_cond_src = cond_src
        skip_registry_temp = (not full_class)
        _cxxcls = cxxcls
        additional_languages = []

    RJWrapperCXXUnit.__name__ = cxxcls.__name__.replace(
        "CXX", "RJWrapperCXX").replace(
            "Unit", "UnitBase")
    return RJWrapperCXXUnit


for x in ['type', 'typedef', 'base_type', 'template', 'template_param',
          'import', 'macro', 'forward_template_param']:
    RJWrapperCXX(x, full_class=True)


class RJWrapperVariableUnit(RJWrapperCXX('var')):

    _src_repl = [
        (r'{PREFIX[=]:rhs}', ''),
    ]


class RJWrapperFunctionUnit(RJWrapperCXX('function')):

    _src_repl = [
        (';', '{\n'),
    ]
    _src_suffix = (
        '  return {name::BS}({name:args});\n'
        '}'
    )


class RJWrapperMethodUnit(RJWrapperCXX('method')):

    _src_repl = [
        ('{const_method}{override};', '{const_method} {\n'),
        ('{api}{virtual}{friend}', ''),
        ('{name}(', '{parent}::{name}('),
    ]
    _src_suffix = (
        '  return _wrap(val_->{name::BS}('
        '{JOIN[, ]:ITER[_unwrap({XXX})]:name:args}'
        '));\n'
        '}'
    )
    _src_fstring_cond_prop = {
        'body': RJWrapperCXX('method')._fstring_cond.replace(
            '{name}', '{name::parent}::{name}'
        ).replace(
            ';', ' {\n'
        ) + (
            '  return val_->{name}('
            '{JOIN[, ]:ITER[_unwrap({XXX})]:name:args});\n'
            '}'
        )
    }


class RJWrapperOperatorUnit(RJWrapperCXX('operator')):

    _src_repl = [
        ('{const_method}{override};', '{const_method} {\n'),
        ('{api}{virtual}{friend}', ''),
        ('operator{name}(', '{parent}::operator{name}('),
    ]
    _src_suffix = (
        '  return _wrap(val_->operator{name::BS}('
        '{JOIN[, ]:ITER[_unwrap({XXX})]:name:args}'
        '));\n'
        '}'
    )


class RJWrapperConstructorUnit(RJWrapperCXX('constructor')):

    _src_repl = [
        (';', ' {\n'),
        ('{api}{virtual}', ''),
        ('{parent}(', '{parent}::{parent}('),
    ]
    _src_suffix = (
        'val_ = new {parent::BS}('
        '{JOIN[, ]:ITER[_unwrap({XXX})]:name:args}'
        ');\n'
        '}'
    )


class RJWrapperDestructorUnit(RJWrapperCXX('destructor')):

    _fstring_cond_src = (
        '{parent}::~{parent}() {}'
    )


class RJWrapperForwardClassUnit(RJWrapperCXX('forward_class')):

    _fstring_cond_src = '{members}'


class RJWrapperHelperFunction(CodeUnit):

    language = 'rjwrapper'
    unit_type = 'helper'
    _properties = [
        'name', 'value', 'template',
    ]
    _properties_optional = [
        'srctype', 'dsttype', 'template_spec'
    ]
    _fstring_cond = (
        '{template}'
        'struct {name}{BOOKEND:template_spec} {\n'
        '  static const bool value = {value};\n'
        '  typedef {dsttype} type;\n'
        '};'
    )
    property_units = dict(
        CodeUnit.property_units,
        srctype='type',
        dsttype='type',
        template_spec='type',
    )

    def __init__(self, *args, **kwargs):
        units = code_unit_registry(language=self.language)
        template = kwargs.get('template', None)
        srctype = kwargs.get('srctype', None)
        dsttype = kwargs.get('dsttype', None)
        if srctype:
            assert dsttype is not None
            kwargs.setdefault('value', 'true')
            if template is None:
                template = units['template'](param=[])
                for x in [srctype, dsttype]:
                    xtemp = x.properties.get('template', None)
                    if xtemp:
                        new_param = []
                        for x in xtemp.properties['param']:
                            xnew = copy.deepcopy(x)
                            xnew.properties.pop('default', None)
                            new_param.append(xnew)
                        template.properties['param'] += new_param
            srctype = srctype.as_type()
            dsttype = dsttype.as_type()
            kwargs.setdefault('template_spec', [srctype])
            kwargs.update(srctype=srctype, dsttype=dsttype)
        else:
            kwargs.setdefault('value', 'false')
            if template is None:
                template = units['template'](
                    param=[units['template_param'](
                        name='T', param_type='typename')])
            if not dsttype:
                Tname = template.properties['param'][0].properties['name']
                kwargs['dsttype'] = units['type'](base=Tname)
        assert template
        kwargs['template'] = template
        super(RJWrapperHelperFunction, self).__init__(*args, **kwargs)


class RJWrapperClassUnit(RJWrapperCXX('class')):

    _fstring_cond_src = (
        '{members}'
    )
    _fstring_cond = (
        '{template}class {name} : public {base_class} {\n'
        'public:\n'
        '  typedef {base_class} BaseType;\n'
        '  typedef {FIRST:template_spec:base} WrappedType;\n'
        '  {name}(WrappedType* val, bool created=false) :\n'
        '    {base_class}(val, created) {}\n'
        '  {name}(WrappedType& val) :\n'
        '    {base_class}(&val) {}\n'
        '  {name}({name}&& rhs) :\n'
        '    {base_class}(rhs) {}\n'
        '  {name}& operator=({name}& rhs) {\n'
        '    {base_class}::operator=(rhs);\n'
        '    return *this;\n'
        '  }\n'
        '{members}\n'
        '};\n'
    )

    def __init__(self, *args, **kwargs):
        type_unit = code_unit_registry(language='rjwrapper')['type']
        kwargs['base_class'] = type_unit(
            base='WrapperBase',
            template_spec=[
                kwargs['wrapped_unit'].as_type(add_module='wrap::')
            ]
        )
        super(RJWrapperClassUnit, self).__init__(*args, **kwargs)


class RJWrapperModuleUnit(RJWrapperCXX('module')):

    _fstring_cond_src = (
        'using {fullname:BS:WR};\n'
        '{members}'
    )


class RJWrapperFileUnit(RJWrapperCXX('file')):

    _properties = RJWrapperCXX('file')._properties + [
        'helpers',
    ]
    ignored_units = [
        'enum', 'enum_value',
    ]
    property_subunits = dict(
        RJWrapperCXX('file').property_subunits,
        helpers='helper',
    )
    _fstring_cond_src = (
        '#include \"{RELPATHC:name:GN}\"\n'
        'template<typename T>\n'
        'WrapperBase<T>::~WrapperBase() {\n'
        '  if (created_val)\n'
        '    delete val_;\n'
        '  val_ = nullptr;\n'
        '}'
    )
    _fstring_cond = (
        '#include <type_traits>\n'
        '{includes}\n'
        '\n'
        '#ifdef WRAP_RAPIDJSON_FOR_DLL\n'
        '\n'
        '{defines}\n'
        '\n'
        'namespace wrap {\n'
        '{FWD:members:WR}\n'
        '}\n'
        '{FWD:members}\n'
        '\n'
        '{helpers}'
        'template<class T>\n'
        'constexpr bool is_wrapper_class_v = is_wrapper_class<T>::value;\n'
        'template<class T>\n'
        'constexpr bool is_wrapped_class_v = is_wrapped_class<T>::value;\n'
        '\n'
        'template<typename T,'
        'std::enable_if_t<!is_wrapper_class<T>::value, bool> = true >\n'
        'T _unwrap(T x) {\n'
        '  return x;\n'
        '}\n'
        'template<typename T,'
        'std::enable_if_t<is_wrapper_class<T>::value, bool> = true >\n'
        'typename is_wrapper_class<T>::type _unwrap(T x) {\n'
        '  return x.val_;\n'
        '}\n'
        'template<typename T,'
        'std::enable_if_t<!is_wrapped_class<T>::value, bool> = true >\n'
        'T _wrap(T x) {\n'
        '  return x;\n'
        '}\n'
        'template<typename T,'
        'std::enable_if_t<is_wrapped_class<T>::value, bool> = true >\n'
        'typename is_wrapped_class<T>::type _wrap(T x) {\n'
        '  return typename is_wrapped_class<T>::type(x);\n'
        '}\n'
        '\n'
        'template<typename T>\n'
        'class WrapperBase {\n'
        'private:\n'
        '  WrapperBase(const WrapperBase&) = delete;\n'
        '  WrapperBase& operator=(const WrapperBase&) = delete;\n'
        'public:\n'
        '  typedef T BaseType;\n'
        '  WrapperBase(T* val, bool created=false) :\n'
        '    val_(val), created_val(created) {}\n'
        '  WrapperBase(T& val) :\n'
        '    WrapperBase(&val) {}\n'
        '  WrapperBase(WrapperBase<T>&& rhs) :\n'
        '    WrapperBase(nullptr) {\n'
        '    *this = rhs;\n'
        # '    std::swap(val_, rhs.val_);\n'
        # '    std::swap(created_val, rhs.created_val);\n'
        '  }\n'
        '  ~WrapperBase();\n'
        '  WrapperBase<T>& operator=(WrapperBase<T>& rhs) {\n'
        '    std::swap(val_, rhs.val_);\n'
        '    std::swap(created_val, rhs.created_val);\n'
        '    return *this;\n'
        '  }\n'
        '  T* val_;\n'
        '  bool created_val;\n'
        '};\n'
        '\n'
        '{members}\n'
        '\n'
        '#else // WRAP_RAPIDJSON_FOR_DLL\n'
        '#endif // WRAP_RAPIDJSON_FOR_DLL\n'
        '\n'
    )

    def __init__(self, *args, **kwargs):
        kwargs.setdefault('helpers', [])
        super(RJWrapperFileUnit, self).__init__(*args, **kwargs)
        new_helpers = [
            RJWrapperHelperFunction(name='is_wrapper_class'),
            RJWrapperHelperFunction(name='is_wrapped_class'),
        ]
        for x in self.iter_child_members():
            if x.unit_type != 'class':
                continue
            wrapped = x.properties['base_class'].properties['template_spec'][0]
            new_helpers += [
                RJWrapperHelperFunction(
                    name='is_wrapper_class',
                    srctype=x, dsttype=wrapped),
                RJWrapperHelperFunction(
                    name='is_wrapped_class',
                    srctype=wrapped, dsttype=x),
            ]
        self.properties['helpers'] = (
            new_helpers + self.properties['helpers'])
