import os
import re
import pprint
from collections import OrderedDict


def camel2underscored(x):
    ignored = ('API', 'JSON')
    out = ''
    i = 0
    while i < len(x):
        c = x[i]
        if c.isupper():
            next_upper = False
            last_upper = False
            if i < (len(x) - 1):
                next_upper = x[i + 1].isupper()
            if i > 0:
                last_upper = x[i - 1].isupper()
            if not (last_upper and next_upper):
                if i != 0 and x[i - 1] != '_':
                    out += '_'
            for m in ignored:
                if x[i:].startswith(m):
                    out += m.lower()
                    i += len(m)
                    break
            else:
                out += c.lower()
                i += 1
        else:
            out += c
            i += 1
    return out


def close_context(a, z, x, pos=None, endpos=None, count=1):
    if pos is None:
        pos = 0
    if endpos is None:
        endpos = len(x)
    while (count > 0) and (pos < endpos):
        if x[pos] == a:
            count += 1
        elif x[pos] == z:
            count -= 1
        pos += 1
    assert count == 0
    return pos


_code_unit_registry = {}


def code_unit_registry(language=None):
    global _code_unit_registry
    if language is not None:
        if language not in _code_unit_registry:
            _code_unit_registry.setdefault(language, {})
        return _code_unit_registry[language]
    return _code_unit_registry


def init_code_unit_registry():
    from generate_generic.cpp import CFileUnit, CXXFileUnit
    from generate_generic.fortran import FortranFileUnit
    classes = [CFileUnit, CXXFileUnit, FortranFileUnit]
    return classes


def get_file_unit(fname):
    init_code_unit_registry()
    global _code_unit_registry
    ext = os.path.splitext(fname)[-1]
    for k, v in code_unit_registry().items():
        if ext in v['file'].ext:
            return v['file']
    raise NotImplementedError(
        f"Could not find file unit for {ext} "
        f"(initialized = {list(code_unit_registry().keys())})")


def register_code_unit(k, v, languages):
    global _code_unit_registry
    for x in languages:
        _code_unit_registry.setdefault(x, {})
        if k in _code_unit_registry[x]:
            if _code_unit_registry[x][k] == v:
                return
            raise RuntimeError(
                f"{_code_unit_registry[x][k]} "
                f"already registered as {k} for {x}. "
                f"\'{v}\' cannot replace it.")
        _code_unit_registry[x][k] = v


class DummyMatch:

    def __init__(self, x, pos=None, endpos=None, groups=None):
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(x)
        if groups is None:
            groups = [(None, x[pos:endpos])]
        self.string = x
        self._pos = pos
        self._endpos = endpos
        self._groups = groups

    def start(self):
        return self._pos

    def end(self):
        return self._endpos

    def group(self, idx):
        if isinstance(idx, str):
            return self.groupdict()[idx]
        return self._groups[idx]

    def groupdict(self):
        return OrderedDict(self._groups)


class CodeUnitMeta(type):

    def __new__(meta, name, bases, class_dict):
        cls = type.__new__(meta, name, bases, class_dict)
        parts = camel2underscored(name).split('_')
        if len(parts) > 2:
            cls.language = parts[0]
            languages = [cls.language] + cls.additional_languages
            if cls.unit_type == 'file':
                register_code_unit('indent', cls.indent, languages)
                register_code_unit('comment', cls.comment, languages)
            register_code_unit(cls.unit_type, cls, languages)
        return cls


class CodeUnit(metaclass=CodeUnitMeta):

    language = None
    additional_languages = []
    unit_type = None
    circular = False
    _regex = None
    _regex_nogroup = None
    _regex_fstring = None
    _fstring = None
    _fstring_cond = None
    _properties = ['name']
    _properties_optional = ['docs', 'type']
    _properties_defaults = {'indent': 0}
    _properties_dont_compare = ['body']
    member_units = []
    member_context = None

    def __init__(self, name=None, match_start=None, match_end=None,
                 check_format=False, **kwargs):
        if name is not None:
            kwargs['name'] = name
        self.match_start = match_start
        self.match_end = match_end
        for k, v in self._properties_defaults.items():
            kwargs.setdefault(k, v)
        missing = []
        for k in self._properties:
            if k not in kwargs:
                missing.append(k)
        if missing:
            raise KeyError(f"The following missing properties are "
                           f"required for {self.__class__}: {missing}\n"
                           f"Provided properties:\n"
                           f"{pprint.pformat(kwargs)}")
        self.properties = {k: kwargs.pop(k) for k in self._properties}
        for k in (self._properties_optional
                  + list(self._properties_defaults.keys())):
            if k in kwargs:
                self.properties[k] = kwargs.pop(k)
        self.unused_properties = kwargs
        # print(f"new {self.__class__}("
        #       f"\n{self.match_start}:{self.match_end}"
        #       f"\n{pprint.pformat(self.all_properties)}\n)")
        if check_format:
            self.test_parse_format()

    def __eq__(self, solf):
        if not isinstance(solf, CodeUnit):
            return False
        for k, v in self.properties.items():
            if k in self._properties_dont_compare:
                continue
            if v != solf.properties.get(k, None):
                if isinstance(v, list):
                    print(k, len(v), len(solf.properties[k]))
                    for i, (vv1, vv2) in enumerate(zip(v, solf.properties[k])):
                        if vv1 != vv2:
                            print(k, i, vv1, vv2)
                            return False
                print(k, v, solf.properties.get(k, None))
                return False
        for k, v in solf.properties.items():
            if k in self._properties_dont_compare:
                continue
            if k not in self.properties:
                print(k, None, v)
                return False
        return True
        # return solf.properties == self.properties

    def __repr__(self):
        return f"{self.__class__.__name__}({self.address})"

    @classmethod
    def get_regex_nogroup(cls):
        if cls._regex_nogroup is None:
            cls._regex_nogroup = cls.get_regex()
            regex = re.compile(r'\?P\<\w+\>')
            match = regex.search(cls._regex_nogroup)
            while match:
                cls._regex_nogroup = cls._regex_nogroup.replace(
                    match.group(0), '?:')
                match = regex.search(cls._regex_nogroup)
        return cls._regex_nogroup

    @classmethod
    def code_units(cls):
        return code_unit_registry(language=cls.language)

    @classmethod
    def get_regex(cls, **kwargs):
        if cls._regex is None:
            if cls._regex_fstring is None:
                raise NotImplementedError(
                    f"No _regex_fstring set for {cls}")
            dont_cache = [False]
            out = cls._regex_fstring

            def do_repl(match):
                if match.groupdict()['mod'] == 'R':
                    dont_cache[0] = True
                    if ((match.groupdict()['group'] not in kwargs
                         and match.groupdict()['param'] == 'W')):
                        # return (f"(?P<{match.groupdict()['group']}>"
                        #         f"{match.groupdict()['param']})")
                        return f"(?P<{match.groupdict()['group']}>\\w+)"
                    return kwargs[match.groupdict()['group']]
                unit = cls.code_units()[match.groupdict()['group']]
                if match.groupdict()['mod'] == 'NG':
                    return unit.get_regex_nogroup()
                elif match.groupdict()['mod']:
                    raise NotImplementedError(
                        f"Unsupported regex mod: "
                        f"{match.groupdict()['mod']}")
                return unit.get_regex()

            out = re.sub(r'\{(?:(?P<mod>\w+)\:)?(?:(?P<param>\w+?)\:)?'
                         r'(?P<group>\w+)\}',
                         do_repl, out)
            if dont_cache[0]:
                return out
            cls._regex = out
        return cls._regex

    def get_fstring(self, **kwargs):
        if self._fstring is not None:
            return self._fstring
        if self._fstring_cond is None:
            raise NotImplementedError(
                f"No _fstring_cond set for {self.__class__}")
        out = self._fstring_cond

        def do_repl(match):
            if match.groupdict()['mod'] == 'C':
                if not kwargs.get(match.groupdict()['group'], ''):
                    return ''
            elif match.groupdict()['mod']:
                raise NotImplementedError(
                    f"Unsupported fstring mod: "
                    f"{match.groupdict()['mod']}")
            return kwargs[match.groupdict()['group']]

        out = re.sub(r'\{(?:(?P<mod>\w+)\:)?(?P<group>\w+)\}',
                     do_repl, out)
        return out

    @property
    def all_properties(self):
        return dict(self.properties, **self.unused_properties)

    @property
    def address(self):
        return self.properties[self._properties[0]]

    @classmethod
    def from_match(cls, match, member_units=None, check_format=False,
                   **kwargs):
        cls.complete_match(match, kwargs, member_units=member_units,
                           check_format=check_format)
        kwargs = dict(match.groupdict(), match=match, **kwargs)
        kwargs.setdefault('match_start', match.start())
        kwargs.setdefault('match_end', match.end())
        kwargs = {k: cls.parse_property(k, v)
                  for k, v in kwargs.items() if v is not None}
        return cls(check_format=check_format, **kwargs)

    @classmethod
    def from_unit(cls, x, **kwargs):
        if isinstance(x, list):
            return [cls.from_unit(xx, **kwargs) for xx in x]
        elif isinstance(x, str):
            return x
        elif isinstance(x, CodeUnit):
            if x.unit_type not in cls.code_units():
                raise NotImplementedError(x.unit_type)
            kwargs = dict(
                {k: cls.from_unit(v) for k, v in x.properties.items()},
                **kwargs)
            return cls.code_units()[x.unit_type](**kwargs)
        return x

    @classmethod
    def parse(cls, x, pos=None, endpos=None, return_match=False,
              member_units=None, check_format=False, **kwargs):
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(x)
        regex = cls.get_regex(**kwargs)
        # print("PARSE", cls, regex)
        pattern = re.compile(regex, flags=re.MULTILINE)
        match = pattern.search(x, pos, endpos)
        if return_match:
            return match
        if match:
            return cls.from_match(match, member_units=member_units,
                                  check_format=check_format, **kwargs)

    @classmethod
    def parse_subunit(cls, x, pos=None, endpos=None, units=None, **kwargs):
        if units is None:
            units = cls.member_units
        match = None
        unit = None
        ipos = pos
        iendpos = endpos
        for iunit in units:
            if isinstance(iunit, str):
                iunit = cls.code_units()[iunit]
            imatch = iunit.parse(x, pos=ipos, endpos=iendpos,
                                 return_match=True, **kwargs)
            if imatch and (match is None or
                           imatch.start() < match.start()):
                match = imatch
                unit = iunit
                iendpos = match.start()
        if match:
            return unit.from_match(match, **kwargs)

    @classmethod
    def parse_subunits(cls, x, pos=None, endpos=None, units=None, **kwargs):
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(x)
        while pos < endpos:
            match = cls.parse_subunit(x, pos=pos, endpos=endpos,
                                      units=units, **kwargs)
            if match:
                yield match
                pos = match.match_end
            else:
                pos = endpos

    @classmethod
    def complete_match(cls, match, kwargs, member_units=None,
                       check_format=False):
        if member_units is None:
            member_units = cls.member_units
        if 'members' in cls._properties:
            assert member_units
        if member_units:
            kwargs.setdefault('member_kwargs', {})
            kwargs['member_kwargs'].setdefault(
                'parent', match.groupdict()['name'])
            kwargs['member_kwargs'].setdefault(
                'check_format', check_format)
        kwargs.setdefault('match_start', match.start())
        kwargs.setdefault('match_end', match.end())
        if ((cls.member_context and 'body' not in kwargs
             and match.group(0).endswith(cls.member_context[0]))):
            endpos_prev = kwargs['match_end']
            endpos = close_context(*cls.member_context, match.string,
                                   pos=endpos_prev)
            kwargs['body'] = match.string[endpos_prev:endpos]
            kwargs['body_start'] = endpos_prev
            kwargs['body_end'] = endpos
            kwargs['match_end'] = endpos
        if member_units and 'members' not in kwargs:
            kwargs['members'] = [
                m for m in cls.parse_subunits(
                    match.string,
                    pos=kwargs.get('body_start', kwargs['match_start']),
                    endpos=kwargs.get('body_end', kwargs['match_end']),
                    units=member_units,
                    **kwargs.get('member_kwargs', {}))]

    @classmethod
    def parse_property(cls, k, x):
        if k == 'indent':
            return int(len(x) / len(cls.code_units()[k]))
        elif k in cls.code_units() and not cls.code_units()[k].circular:
            return cls.code_units()[k].parse(x)
        elif k == 'args':
            if x.startswith('(') and x.endswith(')'):
                x = x[1:-1]
            return list(cls.parse_subunits(x, units=['var']))
        return x

    @classmethod
    def format_property(cls, k, x):
        if k == 'indent':
            assert isinstance(x, int)
            return x * cls.code_units()['indent']
        elif isinstance(x, CodeUnit):
            return x.format()
        elif isinstance(x, list):
            sep = '\n'
            if k == 'args':
                sep = ', '
            return sep.join([cls.format_property(k, xx) for xx in x])
        return x

    def format(self):
        kws = {k: self.format_property(k, v)
               for k, v in self.properties.items()}
        out = self.get_fstring(**kws)
        # print(f"FORMAT:\n{out}")
        return out

    def test_parse_format(self):
        lines = self.format()
        context_properties = []
        if 'parent' in self._properties:
            context_properties.append('parent')
        if self.unit_type == 'file':
            context_properties.append('name')
        kws = {k: self.properties[k] for k in context_properties}
        xalt = self.parse(lines, **kws)
        assert xalt == self


class TypeUnit(CodeUnit):

    unit_type = 'type'


class DocsUnit(CodeUnit):

    unit_type = 'docs'


class VariableUnit(CodeUnit):

    unit_type = 'var'


class FunctionUnit(CodeUnit):

    unit_type = 'function'
    _properties = CodeUnit._properties + [
        'args',
    ]


class MethodUnit(FunctionUnit):

    unit_type = 'method'
    _properties = FunctionUnit._properties + [
        'parent',
    ]

    @property
    def address(self):
        return (f"{self.properties['parent']}::"
                f"{super(MethodUnit, self).address}")


class ClassUnit(CodeUnit):

    unit_type = 'class'
    member_units = ['method']
    _properties = [
        'name', 'members',
    ]


class ModuleUnit(CodeUnit):

    unit_type = 'module'
    member_units = ['module', 'class', 'function']  # , 'var']
    _properties = [
        'name', 'members',
    ]


class FileUnit(CodeUnit):

    unit_type = 'file'
    member_units = ['module', 'class', 'function']  # , 'var']
    _properties = [
        'name', 'members',
    ]
    ext = []
    comment = ''
    indent = ''
    _fstring_cond = (
        '{members}\n'
    )

    @classmethod
    def parse(cls, x, pos=None, endpos=None, return_match=False,
              member_units=None, check_format=False, **kwargs):
        match = DummyMatch(x, pos=pos, endpos=endpos,
                           groups=list(kwargs.items()))
        if return_match:
            return match
        if match:
            return cls.from_match(match, member_units=member_units,
                                  check_format=check_format)

    @classmethod
    def parse_file(cls, name, contents=None, **kwargs):
        if not os.path.splitext(name)[-1] in cls.ext:
            cls = get_file_unit(name)
        if contents is None:
            with open(name, 'r') as fd:
                contents = fd.read()
        return cls.parse(contents, name=name, **kwargs)
