from generate_generic.base import (
    FunctionUnit, MethodUnit, ConstructorUnit, ClassUnit,
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
        '{indent}.method<{WR:type}, {WR:parent}'
        '{PREFIX[, ]:TYPE:WR:args}>'
        '("{name}", &{WR:parent}::{WR:name})'
    )


class JuliaCXXWrapConstructorUnit(JuliaCXXWrapMixin, ConstructorUnit):

    _fstring_cond = (
        '{indent}.constructor<{TYPE:args}>()'
    )


class JuliaCXXWrapClassUnit(JuliaCXXWrapMixin, ClassUnit):

    _fstring_cond = (
        '{indent}mod.add_type<{WR:name}>("{name}")\n'
        '{members};'
    )


class JuliaCXXWrapModuleUnit(JuliaCXXWrapMixin, ModuleUnit):

    member_units = ['class', 'function']
    _fstring_cond = (
        'JLCXX_MODULE define_{name}_module(jlcxx::Module& mod)\n'
        '{\n'
        '  using namespace {SKIPFIRST:WR:unitpath}::{WR:name};\n'
        '{members}\n'
        '}'
    )


class JuliaCXXWrapFileUnit(JuliaCXXWrapMixin, CXXFileUnit):

    language = 'julia_cxxwrap'
    member_units = ['module']
    ignored_units = [
        'macro', 'destructor',
    ]
    _fstring_cond = (
        '#include "jlcxx/jlcxx.hpp"\n'
        '#include "{RELPATHC:WR:name}"\n'
        '{members}\n'
    )


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
