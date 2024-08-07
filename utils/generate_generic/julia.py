from generate_generic import (
    # CodeUnit, TypeUnit, DocsUnit, VariableUnit, FunctionUnit,
    # ClassUnit, MethodUnit,
    FileUnit)


class JuliaFileUnit(FileUnit):

    ext = ['.jl']
    comment = "#"
    indent = 4 * ' '
