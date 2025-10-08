from generate_generic.base import (
    create_mixin, CodeUnit, EnumValueUnit, EnumUnit, FunctionUnit,
    MethodUnit, ConstructorUnit, ClassUnit, ModuleUnit, FileUnit)
from generate_generic.cpp import (
    CXXTypeUnit, CXXVariableUnit, CXXModuleUnit, CXXFileUnit)


RCXXWrapMixin = create_mixin('R_cxxwrap')


class RCXXWrapClassUnit(RCXXWrapMixin, ClassUnit):

    _fstring_cond = (
    )


class RCXXWrapRcppClassConv(RCXXWrapMixin, ClassUnit):

    unit_type = 'rcpp_conv_class'
    _fstring_cond = (
        'template<> SEXP wrap(const {BS:WR:WR:fullname}&);\n'
        'template<> {BS:WR:WR:fullname} as(SEXP);'
    )


class RCXXWrapRcppClassConvDef(RCXXWrapMixin, ClassUnit):

    unit_type = 'rcpp_conv_class_def'
    _fstring_cond = (
        'template<> SEXP wrap(const {BS:WR:WR:fullname}&) {\n'
        '  '
        '}'
    )


class RCXXWrapRcppMod(RCXXWrapMixin, CXXModuleUnit):

    unit_type = 'rcpp_mod'
    member_units = ['rcpp_conv_class']

    def __init__(self, *args, **kwargs):
        kwargs['name'] = 'Rcpp'
        super(RCXXWrapRcppMod, self).__init__(*args, **kwargs)


class RXXWrapFieldUnit(RCXXWrapMixin, FieldUnit):

    _fstring_cond = (
        '.field("{name}", &{BS:WR:parent})'
    )


class RCXXWrapFunctionUnit(RCXXWrapMixin, FunctionUnit):

    _fstring_cond = (
        'function("{name}", &{BS:WR:name}, '
        '{BOOKEND["]["]:C:docs});'
    )


class RCXXWrapMethodUnit(RCXXWrapMixin, MethodUnit):

    _fstring_cond = (
        '.method("{name}", &{BS:WR:parent})'
    )


class RCXXWrapConstructorUnit(RCXXWrapMixin, ConstructorUnit):

    _fstring_cond = (
        '.constructor<{JOIN[, ]:TYPE:ARGS:WR:C:0}>'
        '({BOOKEND["]["]:C:docs})'
    )


class RCXXWrapClassUnit(RCXXWrapMixin, ClassUnit):

    _fstring_cond = (
        'class_<{BS:WR:name}>("{name}")\n'
        '{members}\n'
        ';\n'
    )


class RCXXWrapModuleUnit(RCXXWrapMixin, ModuleUnit):

    _fstring_cond = (
        'RCPP_MODULE({name}) {\n'
        '{members}\n'
        '}'
    )


class RCXXWrapFileUnit(RCXXWrapMixin, CXXFileUnit):

    language = 'R_cxxwrap'
    member_units = ['module']
    ignored_units = [
    ]
    _fstring_cond = (
        '#include <RcppCommon.h>\n'
        '#include "{RELPATHC:WR:name}"\n'
        '{rpp_mod}\n'
        '#include <Rcpp.h>\n'
        '{members}\n'
    )
    wrapper_type = 'SEXP'
    _properties_optional = CXXFileUnit._properties_optional + [
        'rcpp_mod'
    ]
    _properties_defaults_units = dict(
        CXXFileUnit._properties_defaults_units,
        rpp_mod=RCXXWrapRcppMod,
    )

    def __init__(self, *args, **kwargs):
        super(RCXXWrapFileUnit, self).__init__(*args, **kwargs)

        self.instrument_classes()

    def instrument_classes(self):
        for x in self.iter_child_members():
            if x.unit_type != 'class' or not x.wrapped_unit:
                continue
            self.properties['rcpp_mod'].add_member(
                RCXXWrapRcppClassConv.from_unit(
                    x, conversion_prefix='rcpp_conv_'))


class RModuleUnit(ModuleUnit):

    member_units = ['class', 'function']
    _fstring_cond = (
        'mod <- Module("{name}", getDynLib({LIBFILE:GN:parent}))'
    )


class RFileUnit(FileUnit):

    ext = ['.R']
    comment = '#'
    indent = 2 * ' '
    ignored_units = [
    ]
    language_wrapped = 'cxx'
    generating_unit_type = 'R_cxxwrap'
