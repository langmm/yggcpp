from generate_generic.base import (
    CodeUnit, FunctionUnit, MethodUnit, ConstructorUnit, ClassUnit,
    ModuleUnit, FileUnit)
from generate_generic.cpp import CXXTypeUnit, CXXVariableUnit, CXXFileUnit


class JuliaCXXWrapMixin:

    language = 'julia_cxxwrap'

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


class JuliaCXXWrapTypeUnit(JuliaCXXWrapMixin, CXXTypeUnit):

    language = 'julia_cxxwrap'


class JuliaCXXWrapVariableUnit(JuliaCXXWrapMixin, CXXVariableUnit):

    language = 'julia_cxxwrap'


class JuliaCXXWrapFunctionUnit(JuliaCXXWrapMixin, FunctionUnit):

    _fstring_cond = (
        '{indent}}mod.method("{name}", &{WR:name});'
    )


class JuliaCXXWrapMethodUnit(JuliaCXXWrapMixin, MethodUnit):

    _properties = ['name', 'type', 'args', 'parent']
    _fstring_cond = (
        '{indent}.method'
        '<{WR:type}, {BS:WR:parent}{PREFIX[, ]:TYPE:WR:args}>'
        '("{name}", &{BS:WR:parent}::{WR:name})'
    )


class JuliaCXXWrapConstructorUnit(JuliaCXXWrapMixin, ConstructorUnit):

    _fstring_cond = (
        '{indent}.constructor<{TYPE:args}>()'
    )


class JuliaCXXWrapClassUnit(JuliaCXXWrapMixin, ClassUnit):

    _fstring_cond = (
        '{indent}mod.add_type<{WR:name}>'
        '("{name}"'
        '{PREFIX[, jlcxx::julia_base_type<]:SUFFIX[>()]:WR:base_class}'
        ')\n'
        '{members};'
    )
    _properties_optional = ClassUnit._properties_optional + [
        'base_class'
    ]


class JuliaCXXWrapSuperType(JuliaCXXWrapMixin, CodeUnit):

    unit_type = 'super_type'
    _fstring_cond = (
        '{indent}template<> struct SuperType<{WR:fullname}> \n'
        '{ typedef {BS:WR:fullname} type; };'
    )
    _properties = [
        'name', 'base_class'
    ]


class JuliaCXXWrapJLCXXMod(JuliaCXXWrapMixin, CodeUnit):

    unit_type = 'jlcxx_mod'
    member_units = ['super_type']
    _fstring_cond = (
        'namespace jlcxx\n'
        '{\n'
        '{members}\n'
        '}'
    )
    _properties = []
    _properties_optional = [
        'members'
    ]


class JuliaCXXWrapModuleUnit(JuliaCXXWrapMixin, ModuleUnit):

    member_units = ['class', 'function']
    _fstring_cond = (
        'JLCXX_MODULE define_{name}_module(jlcxx::Module& mod)\n'
        '{\n'
        '  using namespace {WR:fullname};\n'
        '{members}\n'
        '}'
    )


class JuliaCXXWrapFileUnit(JuliaCXXWrapMixin, CXXFileUnit):

    language = 'julia_cxxwrap'
    member_units = ['module', 'jlcxx_mod']
    ignored_units = [
        'macro', 'destructor',
    ]
    _fstring_cond = (
        '#include "jlcxx/jlcxx.hpp"\n'
        '#include "{RELPATHC:WR:name}"\n'
        '{members}\n'
    )

    def __init__(self, *args, **kwargs):
        super(JuliaCXXWrapFileUnit, self).__init__(*args, **kwargs)
        self.add_jlcxx()

    def add_jlcxx(self):
        new_members = []
        for x in self.properties.get('members', []):
            for xx in x.properties.get('members', []):
                if xx.unit_type != 'class':
                    continue
                if xx.properties.get('base_class', ''):
                    new_members.append(
                        JuliaCXXWrapSuperType(
                            wrapped_unit=xx.wrapped_unit,
                            generating_unit=xx.generating_unit,
                            base_unit=xx.base_unit,
                            **xx.properties))
        if new_members:
            self.prepend_member(JuliaCXXWrapJLCXXMod(members=new_members))


class JuliaModuleUnit(ModuleUnit):

    _properties = [
        'name', 'members', 'parent',
    ]
    member_units = ['class', 'function']
    _fstring_cond = (
        'module {name}\n'
        '  using CxxWrap\n'
        '  @wrapmodule(() -> "{LIBFILE:GN:parent}")\n'
        '  function __init__()\n'
        '    @initcxx\n'
        '  end\n'
        'end'
    )


class JuliaFileUnit(FileUnit):

    ext = ['.jl']
    comment = "#"
    indent = 4 * ' '
    ignored_units = [
        'macro', 'class', 'method', 'constructor', 'destructor', 'type',
        'var', 'function',
    ]
