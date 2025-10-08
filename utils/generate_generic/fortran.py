import re
from generate_generic.base import (
    DummyMatch, FunctionUnit, ModuleUnit, FileUnit)


class FortranMixin:

    language = 'fortran'

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    @classmethod
    def complete_match_body(cls, match, pos=None, endpos=None):
        out = super().complete_match_body(
            match, pos=pos, endpos=endpos)
        if (not out) and match.group('name'):
            if pos is None:
                pos = match.start()
            if endpos is None:
                endpos = match.end()
            pattern = re.compile(
                (r'^\s*end\s+' + cls.unit_type + r'\s+'
                 + match.group('name') + r'\s*$'))
            m_end = pattern.search(match.string, pos=endpos, endpos=-1)
            if m_end:
                out = DummyMatch(match.string, pos=endpos,
                                 endpos=m_end.end())
        return out


class FortranFunctionUnit(FortranMixin, FunctionUnit):

    _regex_fstring = (
        r'^(?P<indent>\s*)'
        r'(?P<type>{NG:type})\s+{CA:unit_type}\s+(?P<name>\w+)'
        r'(?P<args>\('
        r'(?:\s*\w+\s*(?:\,))*'
        r'(?:\s*(?:\w+)\s*)\))?'
    )


class FortranModuleUnit(FortranMixin, ModuleUnit):

    member_units = ['function', 'subroutine']
    _regex = (
        r'^module\s+(?P<name>\w+)\s*$'
    )
    _fstring_cond = (
        'module {name}\n'
        '{members}\n'
        'end module {name}'
    )


class FortranFileUnit(FortranMixin, FileUnit):

    ext = ['.F90', '.f90', '.F', '.f']
    comment = "!"
    indent = '  '
    language_wrapped = 'c'
