from generate_generic.base import FileUnit


class FortranFileUnit(FileUnit):

    ext = ['.F90', '.f90']
    comment = "!"
    indent = '  '
