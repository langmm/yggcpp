import os
import copy
import pprint
import sys
import pdb
import uuid
from collections import OrderedDict
_base_dir = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
_using_regex = False
try:
    import regex as re
    _using_regex = True
except ImportError:
    import re


if sys.platform == 'darwin':
    _library_prefix = 'lib'
    _library_ext = '.dylib'
elif sys.platform in ['win32', 'cygwin']:
    _library_prefix = ''
    _library_ext = '.dll'
else:
    _library_prefix = 'lib'
    _library_ext = '.so'


class NoDefault:
    pass


def create_mixin(lang):

    class Mixin:

        language = lang

        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)

    return Mixin


def check_regex(pattern, x, include_errors=False):
    result = OrderedDict()
    max_match = 0
    for i in range(len(pattern)):
        key = pattern[:i]
        try:
            regex = re.compile(key)
            result[key] = regex.match(x)
            if result[key]:
                max_match = i
        except BaseException:
            if include_errors:
                result[key] = 'ERROR'
    # pprint.pprint(result)
    print(f"UNMATCHED:\n{pattern[max_match:]}")
    print(f"MAXMATCH: {result[pattern[:max_match]]}")


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


def get_lines(string, pos, padding=0, direction='both'):
    out = ''
    padding_prefix = 1
    padding_suffix = 1
    if direction in ['both', 'before']:
        padding_prefix = padding + 1
    if direction in ['both', 'after']:
        padding_suffix = padding + 1
    out += '\n'.join(string[:pos].rsplit('\n', padding_prefix)[1:])
    out += '\n'.join(string[pos:].split('\n', padding_suffix)[:-1])
    return out


class RegexContext:

    def __init__(self, pattern, context):
        self.pattern = (
            pattern if isinstance(pattern, re.Pattern)
            else re.compile(pattern))
        self.context = context

    def _complete_match(self, k, match):
        if match.end() == match.endpos:
            return match
        match = self.context.complete_match(match)
        if k == 'fullmatch' and match.end() != match.endpos:
            raise ContextMatchError(
                "Completed match does not match the entire string")
        return match

    def _wrap_method(self, k, method):

        def _wrapper(*args, **kwargs):
            out = method(*args, **kwargs)
            if out:
                self._complete_match(k, out)
            return out

        return _wrapper

    def __getattr__(self, k0):
        k = 'match' if k0 == 'fullmatch' else k0
        out = getattr(self.pattern, k)
        if callable(out) and k != 'fullmatch':
            return self._wrap_method(k0, out)
        return out


class DummyMatch:

    _inherited_attributes = [
        'pos', 'endpos', 'lastindex', 'lastgroup', 're',
    ]

    def __init__(self, x, start=None, end=None, groups=None, **kwargs):
        if start is None:
            start = 0
        if end is None:
            end = len(x)
        if groups is None:
            groups = []
        self.string = x
        self._start = start
        self._end = end
        self._groups = []
        for k, v in groups:
            if isinstance(v, str):
                v = DummyMatch.from_substring(x, v)
            self._groups.append((k, v))
        for k in self._inherited_attributes:
            setattr(self, k, kwargs.pop(k, None))
        assert not kwargs

    @classmethod
    def groups_from_match(cls, x):
        groups = []
        for k, v in x.groupdict().items():
            groups.append((k, DummyMatch.from_match(x, idx=k)))
        return groups

    @classmethod
    def from_match(cls, x, idx=0, groups=None, **kwargs):
        if groups is None:
            groups = []
        if idx == 0:
            groups += cls.groups_from_match(x)
        kwargs.setdefault('start', x.start(idx))
        kwargs.setdefault('end', x.end(idx))
        for k in cls._inherited_attributes:
            kwargs.setdefault(k, getattr(x, k))
        return cls(x.string, groups=groups, **kwargs)

    @classmethod
    def from_substring(cls, x, substr, **kwargs):
        idx = x.index(substr)
        return DummyMatch(x, start=idx, end=(idx + len(substr)), **kwargs)

    def group_match(self, idx=0):
        if idx == 0:
            return self
        if isinstance(idx, str):
            for k, v in self._groups:
                if idx == k:
                    return v
            raise KeyError(f'No group with name \'{idx}\'')
        return self._groups[idx - 1]

    def intersects(self, method, start, end=None):
        code_unit = None
        if isinstance(start, CodeUnit):
            code_unit = start
            start = self.adjust_position(
                code_unit.match.start(),
                check_against=code_unit.match.string)
            end = self.adjust_position(
                code_unit.match.end(),
                check_against=code_unit.match.string)
        if end is None:
            end = start
        if method == 'contains':
            return self._contains((self.start(), self.end()),
                                  (start, end))
        elif method == 'contained':
            return self._contains((start, end),
                                  (self.start(), self.end()))
        elif method == 'overlaps':
            return (self._contains((self.start(), self.end()),
                                   (start, start))
                    or self._contains((self.start(), self.end()),
                                      (end, end))
                    or self._contains((start, end),
                                      (self.start(), self.end())))
        else:
            raise ValueError(f"Invalid intersection method: {method}")

    @classmethod
    def _contains(cls, bounds_a, bounds_b):
        return (bounds_a[0] <= bounds_b[0]
                and bounds_a[1] >= bounds_b[1])

    def overlaps(self, start, end=None):
        return self.intersects('overlaps', start, end)

    def contained(self, start, end=None):
        return self.intersects('contained', start, end)

    def contains(self, start, end=None):
        return self.intersects('contains', start, end)

    def start(self, idx=0):
        if idx == 0:
            return self._start
        return self.group_match(idx).start()

    def end(self, idx=0):
        if idx == 0:
            return self._end
        return self.group_match(idx).end()

    def length(self, idx=0):
        return self.end(idx) - self.start(idx)

    def group(self, idx=0):
        match = self.group_match(idx=idx)
        return self.string[match.start():match.end()]

    def groupdict(self):
        out = OrderedDict()
        for k, v in self._groups:
            out[k] = self.group(k)
        return out


class ContextMatchError(ValueError):

    def __init__(self, *args, **kwargs):
        self.start = kwargs.pop('start', None)
        self.match = kwargs.pop('match', None)
        super(ContextMatchError, self).__init__(*args, **kwargs)


class ContextMatch(DummyMatch):

    def __init__(self, start_match, end_match, token, children=None,
                 subparts=None, groups=None, removed_tokens=None):
        assert start_match is not None
        assert end_match is not None
        self.start_match = start_match
        self.end_match = end_match
        self.token = token
        if children is None:
            children = []
        self.children = children
        if subparts is None:
            subparts = []
        self.subparts = subparts
        if removed_tokens is None:
            removed_tokens = []
        self.removed_tokens = removed_tokens
        if groups is None:
            groups = []
        groups += DummyMatch.groups_from_match(self.start_match)
        groups.append(
            ('body', DummyMatch(start_match.string,
                                start=self.start_match.end(),
                                end=self.end_match.start())))
        groups += DummyMatch.groups_from_match(self.end_match)
        super(ContextMatch, self).__init__(
            start_match.string, start=self.start_match.start(),
            end=self.end_match.end(), groups=groups)

    def __repr__(self):
        cls = str(self.__class__).split("'")[1].split('.')[-1]
        out = f"{cls}("
        out += f"{self.token.name}"
        out += f", {self.start()}, {self.end()})"
        return out

    @classmethod
    def from_matches(cls, children, **kwargs):
        assert children
        start_match = kwargs.pop(
            'start_match', DummyMatch(children[0].string, end=0))
        end_match = kwargs.pop(
            'end_match', DummyMatch(children[0].string,
                                    start=len(children[0].string)))
        token = kwargs.pop(
            'token', ContextToken('', '', name='dummy', recursive=False))
        return cls(start_match, end_match, token, children=children,
                   **kwargs)

    def body(self):
        return self.string[self.body_start():self.body_end()]

    def tokens_intersecting(self, method, pos, endpos=None,
                            include_tokens=None):
        code_unit = None
        if isinstance(pos, CodeUnit):
            code_unit = pos
            pos = self.adjust_position(
                code_unit.match.start(),
                check_against=code_unit.match.string)
            endpos = self.adjust_position(
                code_unit.match.end(),
                check_against=code_unit.match.string)
        if endpos is None:
            endpos = pos
        out = []
        if ((((include_tokens is None and self.token.name != 'dummy')
              or self.token.name in include_tokens)
             and self.intersects(method, pos, endpos))):
            out.append(self)
        for child in self.children:
            out += child.tokens_intersecting(method, pos, endpos=endpos,
                                             include_tokens=include_tokens)
        return out

    def tokens_overlapping(self, pos, endpos=None, include_tokens=None):
        return self.tokens_intersecting('overlaps', pos, endpos,
                                        include_tokens=include_tokens)

    def tokens_containing(self, pos, endpos=None, include_tokens=None):
        return self.tokens_intersecting('contains', pos, endpos,
                                        include_tokens=include_tokens)

    def find_last(self, pos, include_tokens=None, token=None,
                  include_overlap=False):
        prev_pos = -1
        if token is not None:
            prev_pos = token.end()
            if include_overlap and token.end() > pos:
                prev_pos = token.body_start()
        if include_tokens is None or self.token.name in include_tokens:
            if self.end() <= pos and self.end() > prev_pos:
                return self
            if ((include_overlap and self.body_start() < pos
                 and self.body_start() > prev_pos)):
                token = self
        for x in self.children:
            token = x.find_last(pos, include_tokens=include_tokens,
                                token=token,
                                include_overlap=include_overlap)
        return token

    def find_next(self, pos, include_tokens=None, token=None,
                  include_overlap=False):
        prev_pos = len(self.string) + 1
        if token is not None:
            prev_pos = token.start()
            if include_overlap and token.start() < pos:
                prev_pos = token.body_end()
        if include_tokens is None or self.token.name in include_tokens:
            if self.start() >= pos and self.start() < prev_pos:
                return self
            if ((include_overlap and self.body_end() >= pos
                 and self.body_end() < prev_pos)):
                token = self
        for x in self.children:
            token = x.find_next(pos, include_tokens=include_tokens,
                                token=token,
                                include_overlap=include_overlap)
        return token

    def compare_adjusted(self, pos_orig, string_orig,
                         nbuf=100, include_tokens=None,
                         verbose=True, **kwargs):
        if include_tokens is None:
            include_tokens = self.removed_tokens
        if not include_tokens:
            raise ValueError("No tokens selected")
        pos_adju = self.adjust_position(pos_orig,
                                        include_tokens=include_tokens,
                                        verbose=verbose, **kwargs)
        string_comp = self.without_tokens(
            pos=pos_adju, remove_tokens=include_tokens,
            verbose=verbose)
        con = self.string[pos_adju:(pos_adju + nbuf)]
        ori = string_orig[pos_orig:(pos_orig + nbuf)]
        adj = string_comp[:nbuf]
        if adj != ori:
            next_token = self.find_next(
                pos_adju, include_tokens=include_tokens,
                include_overlap=True)
            if next_token:
                print('NEXT', pos_adju, next_token.start(), next_token)
                print(next_token.group()[:100])
            print(f'ORIGINAL[{pos_orig}]: \'{ori}\'\n'
                  f'ADJUSTED[{pos_adju}]: \'{adj}\'\n'
                  f'CONTROL[{pos_adju}]: \'{con}\'')
            if ori in string_comp:
                print(f"OFFSET OF ORIGINAL: {string_comp.index(ori)}")
            pdb.set_trace()
        return pos_adju

    def adjust_position(self, res, include_tokens=None,
                        check_against=None, verbose=False,
                        stop_at=None, status=None):
        if include_tokens is None:
            include_tokens = self.removed_tokens
        if not include_tokens:
            raise ValueError("No tokens selected")
        if check_against:
            return self.compare_adjusted(
                res, check_against, include_tokens=include_tokens,
                verbose=verbose, stop_at=stop_at, status=status)
        pos = self.start()
        if status is None:
            status = {'complete': False}

        def extend(val, include=False, name='default',
                   only_advance=False):
            nonlocal pos, res
            if status['complete']:
                return
            if pos < val and pos <= res:
                if not only_advance:
                    width = val - pos
                    if include:
                        if verbose:
                            print(f"ADJUST_POSITION INCLUDING[{name}]: "
                                  f"{pos}:{pos + width}:[{width}]: "
                                  f"{self.string[pos:(pos + width)]}")
                        res += width
                pos = val
            if stop_at and stop_at == (self, name):
                status['complete'] = True

        extend(self.start(), name='START')
        extend(self.body_start(), name='BODY_START',
               include=(self.token.name in include_tokens))
        for child in self.children:
            if status['complete']:
                break
            res = child.adjust_position(res,
                                        include_tokens=include_tokens,
                                        verbose=verbose,
                                        stop_at=stop_at,
                                        status=status)
            extend(child.end(), only_advance=True)
        extend(self.body_end(), name='BODY_END')
        extend(self.end(), name='END',
               include=(self.token.name in include_tokens))
        return res

    def without_tokens(self, pos=None, endpos=None, out='',
                       remove_tokens=None, check_for=None,
                       include_end=False, verbose=False,
                       check_adjusted=False):
        if remove_tokens is None:
            remove_tokens = [self.token.name]
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(self.string)
        if check_adjusted is True:
            check_adjusted = self

        def extend(val, exclude=False, name='default',
                   only_advance=False):
            nonlocal pos, out
            if pos < min(endpos, val):
                if not only_advance:
                    width = (min(endpos, val) - pos)
                    if check_adjusted and verbose:
                        print(80 * '-')
                    if exclude:
                        if verbose and name != 'CHILD_END':
                            print(f"WITHOUT_TOKENS EXCLUDING[{name}]: "
                                  f"{pos}:{min(endpos, val)}:[{width}]: "
                                  f"{self.string[pos:min(endpos, val)]}")
                    else:
                        out += self.string[pos:min(endpos, val)]
                        if verbose and name != 'CHILD_END':
                            print(f"WITHOUT_TOKENS INCLUDING[{name}]: "
                                  f"{pos}:{min(endpos, val)}:[{width}]: "
                                  f"{self.string[pos:min(endpos, val)]}")
                pos = min(endpos, val)
                if check_adjusted and not only_advance:
                    mod = len(out)
                    adj = check_adjusted.adjust_position(
                        mod, include_tokens=remove_tokens,
                        verbose=verbose, stop_at=(self, name))
                    if adj != pos:
                        print(f"pos = {pos}, adj = {adj}, mod = {mod}")
                        pdb.set_trace()
                    assert adj == pos

        extend(self.start(), name='START')
        extend(self.body_start(), name='BODY_START',
               exclude=(self.token.name in remove_tokens))
        for child in self.children:
            out = child.without_tokens(pos=pos, endpos=endpos, out=out,
                                       remove_tokens=remove_tokens,
                                       verbose=verbose,
                                       check_for=check_for,
                                       check_adjusted=check_adjusted)
            extend(child.end(), only_advance=True)
        extend(self.body_end(), name='BODY_END')
        extend(self.end(), name='END',
               exclude=(self.token.name in remove_tokens))
        if include_end:
            extend(endpos, exclude=False, name='ENDPOS')
        # if check_for:
        #     for x in check_for:
        #         if x in out:
        #             print(out)
        #             print(f"Found {x} in string with {remove_tokens} "
        #                   f"tokens removed")
        #             pdb.set_trace()
        #         assert x not in out
        return out

    def body_start(self):
        return self.start_match.end()

    def body_end(self):
        if self.subparts:
            return self.subparts[0].start()
        return self.end_match.start()

    def body_match(self):
        return DummyMatch(self.string, start=self.body_start(),
                          end=self.body_end())

    def get_lines(self, pos, **kwargs):
        return get_lines(self.string, pos, **kwargs)

    def start_line(self, **kwargs):
        return self.get_lines(self.start(), **kwargs)

    def end_line(self, **kwargs):
        return self.get_lines(self.end(), **kwargs)

    def body_start_line(self, **kwargs):
        return self.get_lines(self.body_start(), **kwargs)

    def body_end_line(self, **kwargs):
        return self.get_lines(self.body_end(), **kwargs)

    def summary(self, padding=5, direction='inside', dont_print=False):
        nlines = len(self.group(0).splitlines())
        skip_end = False
        if direction == 'both':
            paddir_start = 'both'
            paddir_end = 'both'
            if nlines <= 2 * padding:
                padding = nlines
                paddir_end = 'after'
        elif direction == 'inside':
            paddir_start = 'after'
            paddir_end = 'before'
            if nlines <= 2 * padding:
                skip_end = True
                padding = nlines + 1
        elif direction == 'outside':
            paddir_start = 'before'
            paddir_end = 'after'
        out = self.start_line(padding=padding, direction=paddir_start)
        if not skip_end:
            out += '\n...\n'
            out += self.end_line(padding=padding, direction=paddir_end)
        if not dont_print:
            print(out)
        return out


class ContextToken:

    match_type = ContextMatch

    def __init__(self, a, z=None, name=None, exclusive=False,
                 recursive=True, use_regex=False, membertokens=None,
                 subtokens=None, symmetric=False):
        self.a = a
        if z is None:
            z = '\n'
        self.z = z
        self.name = name
        if isinstance(self.a, list) or isinstance(self.z, list):
            if isinstance(self.a, list):
                if not use_regex:
                    self.a = [re.escape(x) for x in self.a]
                self.a = '(?:' + '|'.join(self.a) + ')'
            elif not use_regex:
                self.a = re.escape(self.a)
            if isinstance(self.z, list):
                if not use_regex:
                    self.z = [re.escape(x) for x in self.z]
                self.z = '(?:' + '|'.join(self.z) + ')'
            elif not use_regex:
                self.z = re.escape(self.z)
            use_regex = True
        if use_regex:
            if isinstance(self.a, str):
                self.a = re.compile(self.a, flags=re.MULTILINE)
            if isinstance(self.z, str):
                self.z = re.compile(self.z, flags=re.MULTILINE)
        self.exclusive = exclusive
        self.recursive = recursive
        self.symmetric = symmetric
        if self.symmetric:
            self.z = self.a
        self.match_a = self.get_match_function(self.a)
        self.match_z = self.get_match_function(self.z)
        if not membertokens:
            membertokens = []
        self.membertokens = []
        for token in membertokens:
            if isinstance(token, ContextToken):
                pass
            elif isinstance(token, tuple):
                token = ContextToken(*token, use_regex=use_regex)
            else:
                raise TypeError(
                    f"Unsupported member context token: {token}")
            self.membertokens.append(token)
        if not subtokens:
            subtokens = []
        self.subtokens = []
        if self.recursive == 'alternating':
            for x in self.membertokens:
                x.membertokens.append(self)
        elif self.recursive:
            self.membertokens.append(self)
        for token in subtokens:
            if isinstance(token, SubContextToken):
                pass
            elif isinstance(token, ContextToken):
                token = SubContextToken.from_token(token)
            elif isinstance(token, str):
                token = SubContextToken(
                    token, self.z, exclusive=self.exclusive,
                    recursive=False, use_regex=use_regex,
                    membertokens=self.membertokens)
            else:
                raise TypeError(
                    f"Unsupported sub context token: {token}")
            self.subtokens.append(token)

    def __eq__(self, solf):
        if not isinstance(solf, self.__class__):
            return False
        return self.__dict__ == solf.__dict__

    def __repr__(self):
        cls = str(self.__class__).split("'")[1].split('.')[-1]
        out = f"{cls}("
        if self.name:
            out += f"{self.name}"
        else:
            out += f"{self.a}, {self.z}"
            for x in self.subtokens:
                out += f", {x.a}"
        out += ")"
        return out

    @classmethod
    def get_match_function(cls, pattern):
        from functools import partial
        if isinstance(pattern, re.Pattern):
            return partial(ContextToken._match_regex, pattern)
        return partial(ContextToken._match_string, pattern)

    @staticmethod
    def _match_regex(pattern, x, pos, endpos, location='start'):
        if location == 'start':
            return pattern.match(x, pos, endpos)
        elif location == 'end':
            idx = endpos - 1
            while idx > pos:
                match = pattern.match(x, idx, endpos)
                if match:
                    if match.end() == endpos:
                        return match
                    break
            return None
        elif location == 'anywhere':
            return pattern.search(x, pos, endpos)
        else:
            raise ValueError(f"Unsupported location \'{location}\'")

    @staticmethod
    def _match_string(pattern, x, pos, endpos, location='start'):
        idx = -1
        if location == 'start':
            if x[pos:endpos].startswith(pattern):
                idx = 0
        elif location == 'end':
            if x[pos:endpos].endswith(pattern):
                idx = endpos - pos - len(pattern)
        elif location == 'anywhere':
            idx = x[pos:endpos].find(pattern)
        else:
            raise ValueError(f"Unsupported location \'{location}\'")
        if idx != -1:
            pos += idx
            return DummyMatch(x, pos, pos + len(pattern))
        return None

    def find(self, *args, **kwargs):
        kwargs['location'] = 'anywhere'
        return self.match(*args, **kwargs)

    def removeall(self, x, pos=None, endpos=None, skip_tokens=None,
                  return_match=False, **kwargs):
        out = x
        matches = list(self.findall(x, pos=pos, endpos=endpos,
                                    tokens=skip_tokens))
        match = None
        if matches:
            match = ContextMatch.from_matches(
                matches, removed_tokens=[self.name])
            kwargs.setdefault('include_end', True)
            out = match.without_tokens(
                remove_tokens=[self.name], **kwargs)
        if return_match:
            return out, match
        return out

    def findall(self, x, pos=None, endpos=None, tokens=None, **kwargs):
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(x)
        if tokens is None:
            tokens = []
        while pos < endpos:
            match = self.find(x, pos, endpos, tokens=tokens, **kwargs)
            if match:
                yield match
                pos = match.end()
            else:
                break

    def is_complete(self, x, pos=None, endpos=None, **kwargs):
        try:
            list(self.find_all(x, pos, endpos, **kwargs))
        except ContextMatchError:
            return False
        return True

    def complete_match(self, match, **kwargs):
        pos = match.start()
        endpos = match.endpos
        end = match.end()
        for x in self.findall(match.string, pos, endpos,
                              only_start=True, **kwargs):
            xsup = self.match(match.string, x.start(), **kwargs)
            if xsup.end() > end:
                if xsup.end() >= endpos:
                    raise ContextMatchError(
                        f"Match ends at {xsup.end()}, but endpos is "
                        f"{endpos}", start=x, match=xsup)
                end = xsup.end()
        if end > match.end():
            match = DummyMatch.from_match(match, end=end)
        return match

    def match(self, x, pos=None, endpos=None, tokens=None,
              location='start', verbose=False, only_start=False):
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(x)
        if tokens is None:
            tokens = []
        start = self.match_a(x, pos, endpos, location=location)
        if only_start or (not start):
            return start
        summary = get_lines(start.string, start.start(), padding=5,
                            direction='after')
        if self.name in ['comment', 'block_comment']:
            verbose = False
        if verbose:
            print(80 * '=')
            print("BEGINNING MATCH", self)
            print(summary)
            pdb.set_trace()
        children = []
        subparts = []
        pos = start.end()
        if self.exclusive:
            end = self.match_z(x, pos, endpos, location='anywhere')
        else:
            end = None
            while (end is None) and (pos < endpos):
                match = None
                for token in self.subtokens + tokens + self.membertokens:
                    match = token.match(x, pos, endpos, tokens=tokens,
                                        verbose=verbose)
                    if match:
                        if isinstance(token, SubContextToken):
                            subparts.append(match)
                            subparts += match.subparts
                            match.subparts = []
                            end = match.end_match
                        else:
                            children.append(match)
                        pos = match.end()
                        break
                if match is None:
                    end = self.match_z(x, pos, endpos)
                    if end:
                        break
                    pos += 1
        if end is None:
            print(80 * '/')
            print(x[start.start():endpos])
            print(self, start.start(), endpos)
            print("CHILDREN", children[-1], children[-1].end(),
                  children[-1].end_match)
            print("SUBPARTS", subparts)
            pdb.set_trace()
            start_lines = '\n'.join(x[start.start():].splitlines()[:20])
            raise ContextMatchError(
                f"Failed to find end to context {self} beginning with\n"
                f"{start_lines}",
                start=start)
        out = self.match_type(start, end, self, children=children,
                              subparts=subparts)
        if verbose:
            print(80 * '=')
            if '\n' not in out.group():
                print("MATCHED", self, out.group())
            else:
                print("MATCHED", self)
            out.summary()
        return out


class SubContextToken(ContextToken):

    def __init__(self, *args, **kwargs):
        self.repeatable = kwargs.pop('repeatable', True)
        super(SubContextToken, self).__init__(*args, **kwargs)
        if self.repeatable:
            self.subtokens.append(self)

    @classmethod
    def from_token(cls, token):
        return SubContextToken(token.a, token.z,
                               exclusive=token.exclusive,
                               recursive=token.recursive,
                               subtokens=token.subtokens)

    def match(self, *args, **kwargs):
        out = super(SubContextToken, self).match(*args, **kwargs)
        if out and out.subparts:
            out.end_match = out.subparts[-1].end_match
        return out


class CodeUnitFormatterResult:

    def __init__(self, solf, match, pattern, in_test=False, nargs=3,
                 replacements=None, level=None, parent_result=None,
                 **kwargs):
        self.full = match.group()
        # self.orig = solf
        # self.solf = solf
        self.data = solf
        self.data_default = NoDefault
        self.required = False
        self.initial = True
        self.group = None
        self.pattern = pattern
        self.kwargs = kwargs
        self.in_test = in_test
        self.nargs = nargs
        self.match = None
        if replacements is None:
            replacements = OrderedDict()
        self.replacements = replacements
        self.level = level
        self.dont_cache = False
        self.parent_result = parent_result
        self.solf_trace = []
        self.match_trace = []
        if parent_result:
            self.solf_trace += parent_result.solf_trace
        self.solf_trace.append(solf)
        self.update_match(match)

    @property
    def orig(self):
        return self.solf_trace[0]

    @property
    def solf(self):
        out = self.solf_trace[-1]
        if isinstance(out, list):
            return out[0]
        return out

    @solf.setter
    def solf(self, value):
        if value != self.solf_trace[-1]:
            self.solf_trace.append(value)

    @property
    def complete(self):
        return ((not self.match) or (not self.match.group('mod')))

    def check(self):
        if self.in_test:
            print(f"mod = {self.match.group('mod')}, data = {self.data}")
            pdb.set_trace()

    def transfer_data(self, solf):
        self.update(solf.data)
        self.group = solf.group
        self.solf = solf.solf

    @classmethod
    def do_replace(cls, x, level=None, replacements=None):
        prefix = ''
        if level:
            prefix = f"LVL{level}"
        if replacements:
            to_remove = []
            for k, v in reversed(replacements.items()):
                if (not (prefix or k == 'recurse')) or k.startswith(prefix):
                    if k in x:
                        to_remove.append(k)
                    x = x.replace(k, v)
            for k in to_remove:
                replacements.pop(k)
        return x

    def replace(self, x, **kwargs):
        kwargs.setdefault('replacements', self.replacements)
        if self.level is not None:
            kwargs.setdefault('level', self.level - 1)
        return self.do_replace(x, **kwargs)

    def isinstance_unitlist(self, x, allow_empty=False):
        return ((allow_empty or x)
                and isinstance(x, list)
                and all(isinstance(xx, CodeUnit) for xx in x))

    def update_match(self, match):
        self.match = match
        if match:
            self.match_trace.append(match)
            self.mod = match.group('mod')
            self.args = [
                match.group(f'arg{i}') for i in range(self.nargs)
                if match.group(f'arg{i}') is not None
            ]
            self.check()

    def advance(self):
        if self.match.group('add'):
            add = self.match.group('add')
            self.update_match(self.pattern.fullmatch(add))
            if not self.match:
                raise ValueError(f"Failed to find mod in \'{add}\'")
        else:
            self.match = None
        self.initial = False

    def update(self, data):
        self.data = data


class CodeUnitFormatter:

    _arg = r'[^\[\]\{\}]*'

    def __init__(self, nargs=3):
        arg = self._arg
        if _using_regex:
            arg = (
                r'(?:(?:' + self._arg + r')|'
                r'(?:' + self._arg + r'(?R)' + self._arg + r'))'
            )
        self.string_sub = (
            r'(?P<add>(?:\w+(?:\[' + arg + r'\]){0,' + str(nargs)
            + r'}\:)+)?(?:(?P<mod>\w+)'
        )
        for i in range(nargs):
            self.string_sub += (
                r'(?:\[(?P<arg' + str(i) + r'>' + arg + r')\])?'
            )
        self.string_sub += r')'
        self.string_full = (
            r'\{' + self.string_sub + r'\}'
        )
        self.pattern_sub = re.compile(self.string_sub + r'\:')
        self.pattern_full = re.compile(self.string_full)

    def sub(self, solf, x, dont_cache=None, parent_result=None, **kws):
        x0 = x
        replacements = OrderedDict()
        level = 0

        def do_repl(match):
            result = CodeUnitFormatterResult(
                solf, match, self.pattern_sub,
                parent_result=parent_result,
                replacements=replacements, level=level, **kws)
            out = self.replacements(result)
            return out

        class PreserveUnitsError(TypeError):

            def __init__(self, unit):
                self.unit = unit
                super(PreserveUnitsError, self).__init__(
                    f"Preserving unit: {unit}")

        def do_sub(match):
            result = CodeUnitFormatterResult(
                solf, match, self.pattern_sub,
                replacements=replacements, **kws)
            out = self.substring(result)
            if result.dont_cache and dont_cache:
                dont_cache[0] = result.dont_cache
            if not isinstance(out, str):
                raise PreserveUnitsError(out)
            return out

        while self.pattern_full.search(x):
            level += 1
            x = self.pattern_full.sub(do_repl, x)
        if level <= 1 and 'recurse' not in replacements:
            replacements = OrderedDict()
            x = x0
        else:
            x = CodeUnitFormatterResult.do_replace(
                x, level=level, replacements=replacements)
            level -= 1
        last = None
        while isinstance(x, str) and self.pattern_full.search(x):
            assert x != last
            last = x
            try:
                x = self.pattern_full.sub(do_sub, x)
                x = CodeUnitFormatterResult.do_replace(
                    x, level=level, replacements=replacements)
            except PreserveUnitsError as e:
                x = e.unit
                break
            level -= 1
        if isinstance(x, str):
            assert not self.pattern_full.search(x)
            if replacements and 'recurse' in replacements:
                x = self.recursion(x, replacements.pop('recurse'))
        return x

    def replacements(self, result):
        idstr = f"LVL{result.level}{uuid.uuid4()}"
        assert idstr not in result.match.string
        assert idstr not in result.replacements
        result.replacements[idstr] = result.full
        return idstr

    @classmethod
    def _recurse(cls, x, idstr, match, orig_match):
        raise NotImplementedError

    def recursion(self, x, idstr):
        pattern = re.compile(re.escape(idstr) +
                             r'\[(?P<count>\d+)\]'
                             r'\[(?P<final>[^\]]*?)\]')
        match = pattern.search(x)
        orig_match = match
        while match:
            assert not pattern.search(match.group('final'))
            x = self._recurse(x, idstr, match, orig_match)
            match = pattern.search(x)
        return x

    def substring(self, result):
        while not result.complete:
            if not self._parse(result):
                self.parse(result)
            result.advance()
        return self.format(result)

    def format(self, result):
        raise NotImplementedError

    def _parse(self, result, mod=None, args=None):
        if mod is None:
            mod = result.mod
            args = result.args
        elif args is None:
            args = []
        if mod == 'TEST' or (result.data and mod == 'CTEST'):
            result.in_test = True
        elif mod == "CU":
            assert len(args) == 1
            result.data = result.data.code_unit(
                args[0], result.data_default)
        elif mod == "C":
            result.data_default = None
        elif mod == "R":
            assert result.initial
            assert len(result.args) == 2
            assert not result.match.group('add')
            kws = dict(result.kwargs, no_group=True)
            final = self.sub(result.orig, result.replace(result.args[1]),
                             in_test=result.in_test, **kws)
            if '?P<' in final:
                pdb.set_trace()
            assert '?P<' not in final
            assert '[' not in final
            if _using_regex:
                if 'recurse' not in result.replacements:
                    result.replacements['recurse'] = (
                        f'(?:(?:{final})|(?R))')
                idstr = result.replacements['recurse']
            else:
                if 'recurse' not in result.replacements:
                    result.replacements['recurse'] = (
                        f"RECURSE{uuid.uuid4()}")
                idstr = (result.replacements['recurse'] +
                         f"[{result.args[0]}]"
                         f"[{final}]")
            result.update(idstr)  # f"(?:(?:{final})|(?:{idstr}))")
        elif mod == "REQ":
            result.required = True
        elif mod in ['OR', 'AND', 'IF', 'IFNOT', 'IFEQ', 'IFNEQ']:
            if mod in ['IF', 'IFNOT']:
                assert len(args) == 1 or len(args) == 2
            elif mod in ['IFEQ', 'IFNEQ']:
                assert len(args) == 2 or len(args) == 3
            cond = []
            for i in range(len(args)):
                try:
                    cond.append(
                        self.sub(result.orig, result.replace(args[i]),
                                 in_test=result.in_test, **result.kwargs)
                    )
                except UnitFormatError:
                    cond.append('')
            if (((mod == 'OR' and not any(cond))
                 or (mod == 'AND' and not all(cond))
                 or (mod == "IF" and not cond[0])
                 or (mod == "IFNOT" and cond[0])
                 or (mod == "IFEQ" and (cond[0] != cond[1]))
                 or (mod == "IFNEQ" and (cond[0] == cond[1])))):
                if (mod in ['IF', 'IFNOT']) and len(args) == 2:
                    result.update(cond[-1])
                elif (mod in ['IFEQ', 'IFNEQ']) and len(args) == 3:
                    result.update(cond[-1])
                else:
                    result.update(None)
        elif mod == 'NULL':
            result.update(None)
        elif mod == 'STR':
            assert len(args) == 1
            result.update(args[0])
        elif mod == 'SUB':
            assert len(args) == 1
            try:
                result.update(
                    self.sub(result.data, result.replace(args[0]),
                             in_test=result.in_test, **result.kwargs)
                )
            except UnitFormatError:
                result.update(None)
        elif mod == 'ATTR':
            assert len(args) == 1
            if result.data_default != NoDefault:
                result.data = getattr(result.data, args[0],
                                      result.data_default)
            else:
                result.data = getattr(result.data, args[0])
        elif mod == 'REPLACE_TYPES':
            assert len(args) == 1
            result0 = self.sub(result.orig, result.replace(args[0]),
                               in_test=result.in_test, **dict(
                                   result.kwargs, return_result=True))
            if isinstance(result0.data, list):
                for x in result0.data:
                    x.replace_types(result.data)
            elif isinstance(result0.data, CodeUnit):
                result0.data.replace_types(result.data)
            result.transfer_data(result0)
        elif mod in ['ADD', 'SUBTRACT']:
            assert len(args) == 1
            if isinstance(result.data, list):
                data0 = result.data[0]
            else:
                data0 = result.data
            assert isinstance(data0, (int, float))
            val = type(data0)(float(args[0]))
            if mod == 'SUBTRACT':
                val *= -1
            if isinstance(result.data, list):
                result.data = [x + val for x in result.data]
            else:
                result.data += val
        elif mod in ['RELPATHC', 'LIBFILE', 'BASEFILE',
                     'PREFIX', 'SUFFIX', 'CPREFIX', 'CSUFFIX',
                     'BOOKEND', 'CBOOKEND']:
            if result.data is None:
                return True
            self.format(result)
            if mod == 'RELPATHC':
                if not os.path.isabs(result.data):
                    result.data = os.path.join(_base_dir, result.data)
                result.data = os.path.relpath(
                    result.data,
                    start=os.path.join(_base_dir, 'cpp', 'include'))
            elif mod == 'LIBFILE':
                base = os.path.splitext(os.path.basename(result.data))[0]
                result.data = f"{_library_prefix}{base}{_library_ext}"
            elif mod == 'BASEFILE':
                result.data = os.path.basename(result.data)
            elif mod == 'BOOKEND' or (mod == 'CBOOKEND' and result.data):
                assert len(args) <= 2
                if result.group in result.solf.list_bounds:
                    group = result.group
                    if len(args) == 0:
                        args.append(
                            result.solf.list_bounds[group][0] + ' ')
                    if len(args) == 1:
                        args.append(
                            ' ' + result.solf.list_bounds[group][1])
                assert len(args) == 2
                result.data = args[0] + result.data + args[1]
            elif mod == 'PREFIX' or (mod == 'CPREFIX' and result.data):
                assert len(args) == 1
                result.data = args[0].replace('\\n', '\n') + result.data
            elif mod == 'SUFFIX' or (mod == 'CSUFFIX' and result.data):
                assert len(args) == 1
                result.data = result.data + args[0].replace('\\n', '\n')
        elif mod in ['SKIPFIRST', 'JOIN', 'ITER', 'RITER', 'UNIQUE',
                     'SELECT', 'FIRST', 'LAST', 'INDEX']:
            if result.data is None:
                return True
            assert isinstance(result.data, list)
            if mod == 'SKIPFIRST':
                if result.data:
                    result.data = result.data[1:]
                else:
                    result.data = result.default_data
            elif mod == 'JOIN':
                assert len(args) == 1
                result.data = args[0].join(result.data)
            elif mod in ['ITER', 'RITER']:
                assert len(args) == 1
                ifmt = args[0]
                if mod == 'RITER':
                    vals = result.data[::-1]
                else:
                    vals = result.data
                # if mod == 'ITER':
                #     print(mod, ifmt, result.replace(ifmt))
                #     pdb.set_trace()
                result.data = [
                    self.sub(result.orig, result.replace(ifmt),
                             XXX=idata, in_test=result.in_test)
                    for idata in vals
                ]
            elif mod == 'UNIQUE':
                vals = result.data
                result.data = []
                for x in vals:
                    if x not in result.data:
                        result.data.append(x)
            elif mod == 'SELECT':
                vals = result.data
                result.data = []
                for x in vals:
                    if isinstance(x, CodeUnit) and x.unit_type in args:
                        result.data.append(x)
            elif mod in ['FIRST', 'LAST', 'INDEX']:
                if mod == 'INDEX':
                    assert len(args) == 1
                    idx = args[0]
                elif mod == 'FIRST':
                    idx = 0
                elif mod == 'LAST':
                    idx = -1
                if idx >= len(result.data) or len(result.data) == 0:
                    result.data = result.default_data
                else:
                    result.data = result.data[idx]
        elif (isinstance(result.data, list) and len(result.data) == 0):
            pass
        else:
            return False
        return True


class FstringFormatter(CodeUnitFormatter):

    def format(self, result):
        if result.data is None:
            result.data = ''
        if result.kwargs.get('return_result', False):
            return result
        if not result.kwargs.get('preserve_units', False):
            result.data = result.solf.format_property(
                result.group, result.data, parent_result=result)
        return result.data

    def extract(self, result, key=None, korig=None, value=NoDefault):
        if key is None:
            key = result.group
        else:
            result.group = key
        if not result.initial:
            assert key != '0'
        if korig is None:
            korig = key
        if ((isinstance(result.data, CodeUnit)
             or result.isinstance_unitlist(result.data))):
            result.solf = result.data
        if ((key in result.solf._properties_optional
             and not result.required)):
            result.data_default = None
        try:
            if value != NoDefault:
                result.data = value
            elif isinstance(result.data, CodeUnit):
                result.data = result.data.get_property(
                    key, default=result.data_default, **result.kwargs)
            elif result.isinstance_unitlist(result.data):
                result.data = [x.get_property(
                    key, default=result.data_default, **result.kwargs)
                               for x in result.data]
                result.data = [x for x in result.data if x is not None]
            elif result.data is None and result.data_default != NoDefault:
                return
            else:
                raise TypeError(
                    f"Cannot extract data from {type(result.data)} "
                    f"object {result.data}")
        except BaseException as e:
            msg = (
                f"Failed to extract property \'{korig}\' for use in "
                f"\'{result.full}\' format string for "
                f"{result.orig.language}"
                f" {result.orig.unit_type}.\nERROR[{type(e)}] = {e}"
            )
            raise UnitFormatError(msg)
        if isinstance(result.data, CodeUnit):
            result.solf = result.data

    def parse(self, result, mod=None, args=None):
        _attr_aliases = {
            "PR": 'parent_unit',
            "BS": 'base_unit',
            "WR": 'wrapped_unit',
            "GN": 'generating_unit',
        }
        _prop_aliases = {
            "BC": 'base_class_unit',
            "CC": 'child_class_unit',
            "TL": 'top_level',
            "FWD": 'forward_unit',
        }
        if mod is None:
            mod = result.mod
            args = result.args
        elif args is None:
            args = []
        if mod == "0":
            assert result.initial
        elif ((mod in _attr_aliases
               or (mod and mod.lower() in result.orig.related_units))):
            assert (isinstance(result.data, CodeUnit)
                    or result.isinstance_unitlist(result.data,
                                                  allow_empty=True))
            if mod.lower() in result.orig.related_units:
                self.extract(result, key=mod.lower(), korig=mod)
            else:
                if isinstance(result.data, list):
                    val = [getattr(x, _attr_aliases[mod])
                           for x in result.data]
                else:
                    val = getattr(result.data, _attr_aliases[mod])
                if val:
                    self.extract(result, key=_attr_aliases[mod],
                                 korig=mod, value=val)
        elif mod in _prop_aliases:
            self.extract(result, key=_prop_aliases[mod], korig=mod)
        else:
            self.extract(result, key=mod, korig=mod)


class RegexFormatter(CodeUnitFormatter):

    @classmethod
    def _recurse(cls, x, idstr, match, orig_match):
        count = int(float(match.group('count')))
        final = match.group('final')
        assert '?P<' not in final
        if count == 0:
            xalt = final
        else:
            count -= 1
            xalt = cls.strip_names(
                orig_match.string.replace(orig_match.group(),
                                          f"{idstr}[{count}][{final}]"))
            # xalt = f"(?:(?:{final})|(?:{xalt}))"
        assert '?P<' not in xalt
        x = x.replace(match.group(), xalt)
        # print("RECURSE", match)
        # pdb.set_trace()
        return x

    @classmethod
    def strip_names(cls, x):
        out = x
        regex = re.compile(r'\?P\<\w+\>')
        match = regex.search(out)
        while match:
            out = out.replace(match.group(0), '?:')
            match = regex.search(out)
        return out

    def substring(self, result):
        result.data = None
        return super(RegexFormatter, self).substring(result)

    def format(self, result, **kwargs):
        if result.group is not None:
            if result.group == result.orig.unit_type:
                final = result.orig.get_regex(
                    no_recurse=True, no_group=True)
                self.update(result, f'{{R[2][{final}]}}')
            else:
                self.update(
                    result,
                    result.solf.code_unit(result.group,
                                          default=result.data_default))
        if not isinstance(result.data, str):
            if result.solf:
                kwargs = dict(kwargs, **result.kwargs)
                result.data = result.solf.get_regex(**kwargs)
            else:
                result.data = ''
        return result.data

    def update(self, result, x):
        if iscodeunit_class(x):
            if x.optional or 'regex' in x.dont_cache:
                result.dont_cache = True
            result.solf = x
        elif isinstance(x, str):
            result.data = x
        else:
            raise TypeError(f"Value must be regex or unit not "
                            f"{type(x)} \"{x}\"")
        result.group = None

    def parse(self, result):
        args = result.args
        mod = result.mod
        if result.initial:
            result.group = result.mod
            return
        elif mod == 'KWS':
            assert len(args) <= 1
            result.dont_cache = True
            if (result.group not in result.kwargs) and args:
                val = f"(?P<{result.group}>{args[0]})"
            else:
                val = result.kwargs[result.group]
            self.update(result, val)
        elif mod == 'CLS':
            assert len(args) <= 1
            if (not hasattr(result.solf, result.group)) and args:
                val = f"(?P<{result.group}>{args[0]})"
            else:
                val = getattr(result.solf, result.group)
            self.update(result, val)
        elif mod == "NG":
            assert not args
            result.kwargs['no_group'] = True
        elif mod == "NR":
            assert not args
            result.kwargs['no_recurse'] = True
        elif mod in ["CONTAINER", "RCONTAINER"]:
            # assert len(args) == 2 or len(args) == 3
            assert len(args) <= 3
            suffix = ''
            if mod != "RCONTAINER":
                suffix = '?'
            group = result.group
            sep = re.escape(result.solf.list_seps[group].strip())
            if len(args) == 0:
                args.append(None)
                if group not in result.solf.property_subunits:
                    print(f'MISSING property_subunits entry for '
                          f'{group} on class {result.solf}: '
                          f'{result.solf.property_subunits}')
                    pdb.set_trace()
                subunit = result.solf.property_subunits[group]
                if isinstance(subunit, list):
                    subunit_data = '|'.join(f"(?:{{NG:{x}}})"
                                            for x in subunit)
                    subunit_data = f'(?:{subunit_data})'
                else:
                    subunit_data = f'{{NG:{subunit}}}'
                # result.group = result.solf.property_subunits[group]
                # subunit = self.format(result)
                result.data = (
                    f'(?:\\s*{subunit_data}\\s*{sep})*'
                    f'(?:\\s*{subunit_data}){suffix}\\s*')
            else:
                result.data_default = args[0]
                self.format(result)
            if len(args) == 1:
                args.append(re.escape(
                    result.solf.list_bounds[group][0]))
                args[1] += r'\s*'
            if len(args) == 2:
                args.append(re.escape(
                    result.solf.list_bounds[group][1]))
                args[2] = r'\s*' + args[2]
            self.update(result,
                        f"(?P<{group}_CONTAINER>{args[1]}"
                        f"(?P<{group}>{result.data})"
                        f"{args[2]}){suffix}")
        elif mod:
            raise NotImplementedError(f"Unsupported regex mod: {mod} in "
                                      f"\'{result.match.string}\'")


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
    from generate_generic.julia import JuliaCXXWrapFileUnit, JuliaFileUnit
    from generate_generic.rjwrapper import RJWrapperFileUnit
    classes = [
        CFileUnit, CXXFileUnit, FortranFileUnit,
        JuliaCXXWrapFileUnit, JuliaFileUnit,
        RJWrapperFileUnit,
    ]
    return classes


def get_file_unit_class(fname, language=None):
    init_code_unit_registry()
    global _code_unit_registry
    if language is not None:
        out = code_unit_registry(language)['file']
        out.reset_class_properties()
        return out
    ext = os.path.splitext(fname)[-1]
    for k, v in code_unit_registry().items():
        if ext in v['file'].ext:
            v['file'].reset_class_properties()
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


_file_unit_registry = {}


def register_file_unit(v):
    global _file_unit_registry
    k = v.properties['name']
    if k in _file_unit_registry:
        raise RuntimeError(f"File unit for {k} already registered")
    _file_unit_registry[k] = v


def get_file_unit(name, language=None, contents=None,
                  dont_generate=False, dont_read=False, **kwargs):
    global _file_unit_registry
    if os.path.isabs(name):
        fullname = name
        name = os.path.relpath(name, _base_dir)
    else:
        fullname = os.path.join(_base_dir, name)
    if name in _file_unit_registry:
        return copy.deepcopy(_file_unit_registry[name])
    if not dont_generate:
        cls = get_file_unit_class(name, language=language)
        if dont_read:
            out = cls(name=name, **kwargs)
        else:
            if contents is None:
                with open(fullname, 'r') as fd:
                    contents = fd.read()
            out = cls.parse(contents, name=name, **kwargs)
        return out
    raise KeyError(f"No file unit registered for {name}")


class CodeUnitMeta(type):

    def __new__(meta, name, bases, class_dict):
        cls = type.__new__(meta, name, bases, class_dict)
        base_unit_type = None
        for base in bases:
            base_unit_type = getattr(base, 'unit_type', None)
        temp_skip = getattr(cls, 'skip_registry_temp', False)
        if temp_skip:
            delattr(cls, 'skip_registry_temp')
        if (((not temp_skip)
             and (cls.language is not None
                  or (cls.unit_type is not None
                      and cls.unit_type == base_unit_type)))):
            cls._generated_class_properties = []
            for k in ['_regex', '_regex_nogroup', '_fstring',
                      '_regex_norecurse']:
                if getattr(cls, k, None) is None:
                    setattr(cls, k, None)
                if getattr(cls, f'{k}_fstring', None) is None:
                    setattr(cls, f'{k}_fstring', None)
            if ((cls.member_units
                 and ('members' not in
                      cls._properties + cls._properties_optional))):
                cls._properties_optional = cls._properties_optional + [
                    'members'
                ]
            for x in cls.parallel_units:
                if x not in cls.related_units:
                    cls.related_units.append(x)
            if cls.language is None:
                parts = camel2underscored(name).split('_')
                cls.language = parts[0]
            for x in cls.ignored_units:
                x_attr = {
                    'unit_type': x,
                    'language': cls.language,
                    'additional_languages': cls.additional_languages,
                    '_properties': [],
                    '_properties_optional': [],
                    '_fstring': '',
                    'is_dummy_unit': True,
                    '_regex': '',
                }
                type(f'{cls.language.title()}{x.title()}Unit',
                     (CodeUnit, ), x_attr)
            languages = [cls.language] + cls.additional_languages
            if cls.member_context and not cls.member_context_token:
                token_kws = {
                    'recursive': (cls.unit_type in cls.member_units)}
                token_kws['membertokens'] = [
                    cls.code_unit(x).member_context_token
                    for x in cls.member_units
                    if (x != cls.unit_type
                        and cls.code_unit(x).member_context_token)
                ]
                if not token_kws['membertokens']:
                    token_kws['membertokens'].append(
                        ContextToken(*cls.member_context, name='code',
                                     recursive=True))
                cls.member_context_token = ContextToken(
                    *cls.member_context, name=cls.unit_type,
                    **token_kws)
            if ((getattr(cls, '_regex_norecurse', None) is None
                 and getattr(cls, '_regex_norecurse_fstring', None) is None)):
                if getattr(cls, '_regex', None):
                    setattr(cls, '_regex_norecurse',
                            getattr(cls, '_regex'))
                elif getattr(cls, '_regex_fstring', None):
                    setattr(cls, '_regex_norecurse_fstring',
                            getattr(cls, '_regex_fstring'))
            if cls.unit_type == 'file':
                cls.context_tokens = {
                    k: v for k, v in cls.context_tokens.items()}
                cls.context_tokens['string'] = ContextToken(
                    r'(?<!\\)[\'\"]', name='string',
                    exclusive=True, use_regex=True, symmetric=True)
                cls.context_tokens['fstring'] = ContextToken(
                    r'(?<!\\)\{', r'(?<!\\)\}', use_regex=True,
                    recursive='alternating',
                    name='fstring', membertokens=[ContextToken(
                        r'(?<!\\)\[', r'(?<!\\)\]', use_regex=True,
                        recursive=True)])
                if cls.comment:
                    cls.context_tokens['comment'] = ContextToken(
                        cls.comment, name='comment', exclusive=True)
                if cls.block_comment:
                    cls.context_tokens['block_comment'] = ContextToken(
                        *cls.block_comment, name='block_comment',
                        exclusive=True)
                register_code_unit('context_tokens', cls.context_tokens,
                                   languages)
                register_code_unit('indent', cls.indent, languages)
                register_code_unit('comment', cls.comment, languages)
                register_code_unit('block_comment', cls.block_comment,
                                   languages)
                register_code_unit('ignore_blocks', cls.ignore_blocks,
                                   languages)
                register_code_unit('modsep', cls.modsep, languages)
                register_code_unit('libext', _library_ext, languages)
                register_code_unit('libprefix', _library_prefix, languages)
            cls.add_subunits()
            cls._before_registration()
            register_code_unit(cls.unit_type, cls, languages)
        return cls


class InvalidUnitType(KeyError):
    pass


class UnitFormatError(KeyError):
    pass


def iscodeunit_class(x):
    return (isinstance(x, type) and issubclass(x, CodeUnit))


class CodeUnit(metaclass=CodeUnitMeta):

    language = None
    additional_languages = []
    unit_type = None
    circular = False
    optional = False
    fstring_formatter = FstringFormatter()
    allow_duplicates = False
    regex_formatter = RegexFormatter()
    no_forward_unit = False
    _regex = None
    _regex_fstring = None
    _regex_fstring_norecurse = None
    _regex_nogroup = None
    _regex_nogroup_fstring = None
    _fstring = None
    _fstring_cond = None
    _fstring_cond_prop = {}
    _noindent_flag = '\\NOINDENT'
    _properties = ['name']
    _properties_optional = [
        'docs', 'type', 'parent', 'unitpath',
        'members_for_parent', 'members_for_parent_used',
        'external_types', 'top_level', 'subunit_index',
        'skipped',
    ]
    _properties_defaults = {
        'indent': 0,
        'libext': _library_ext,
        'libprefix': _library_prefix,
        'unitpath': [],
        'skipped': False,
    }
    _properties_defaults_units = {}
    _properties_from_parent = [
        'indent', 'unitpath', 'member_index',
    ]
    _properties_dont_copy = [
        'members_for_parent', 'members_for_parent_used',
        'child_class_unit',
    ]
    _properties_references = [
        'base_class_unit', 'forward_unit', 'parent',
        'top_level', 'specialized_type',
    ]
    _properties_dont_compare = [
        'body', 'indent', 'unitpath', 'parent', 'member_index',
        'child_class', 'external_types',
        'base_class', 'base_class_unit', 'forward_unit',
    ]
    _recursive_properties = [
        'child_class_unit', 'top_level', 'specialized_type',
    ]
    child_indent = 1
    member_units = []
    member_context = None
    member_context_token = None
    ignored_units = []
    is_dummy_unit = False
    _generated_class_properties = []
    dont_cache = []
    list_seps = {
        'members': '\n',
        'args': ', ',
        'type': ', ',
        'instant_spec': ', ',
        'template_spec': ', ',
        'base_template_spec': ', ',
        'specialized_template_spec': ', ',
    }
    list_bounds = {
        'args': ('(', ')'),
        'instant_spec': ('<', '>'),
        'template_spec': ('<', '>'),
        'base_template_spec': ('<', '>'),
        'specialized_template_spec': ('<', '>'),
    }
    address_property = None
    parallel_units = ['base_unit', 'wrapped_unit', 'generating_unit']
    related_units = ['parent_unit', 'file_unit']
    property_units = {}
    property_subunits = {}
    members_discontiguous = False

    def __init__(self, name=None, match=None, match_start=None,
                 match_end=None, match_string=None, check_format=False,
                 verbose=False, **kwargs):
        if name is not None:
            kwargs['name'] = name
        self.match = match
        self.match_start = match_start
        self.match_end = match_end
        self.match_string = match_string
        for k in self.related_units:
            setattr(self, k, kwargs.pop(k, None))
        self.exclude_from_children = False
        self.added_properties_optional = ['member_index']
        for k, v in self._properties_defaults.items():
            kwargs.setdefault(k, v)
        for k, v in self._properties_defaults_units.items():
            kwargs.setdefault(k, v())
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
                  + list(self._properties_defaults.keys())
                  + self.added_properties_optional):
            if k in kwargs:
                self.properties[k] = kwargs.pop(k)
            elif f"{k}_CONTAINER" in self.properties:
                self.properties[f"{k}_CONTAINER"] = kwargs.pop(
                    f"{k}_CONTAINER", [])
        self.unused_properties = kwargs
        # properties = self.properties
        # self.properties = {}
        # property_order = [
        #     k for k in self._properties + self._properties_optional
        #     if k in properties]
        # for k in properties.keys():
        #     if k not in property_order:
        #         property_order.append(k)
        # for k in property_order:
        #     self.set_property(k, properties[k])
        members = self.properties.pop('members', [])
        self.add_members(members)
        for k in list(self.properties.keys()):
            self.transfer_properties_for_parent(k, self.properties[k])
        if verbose:
            print(f"new {self.__class__}("
                  f"\n{self.match_start}:{self.match_end}"
                  f"\n{pprint.pformat(self.all_properties)}\n)")
        if check_format:
            self.test_parse_format()

    def __eq__(self, solf):
        return self.compare(solf)

    def __deepcopy__(self, memo):
        properties_preserved = {}
        properties = {}
        for k in self._properties + self._properties_optional:
            if ((k not in self.properties
                 or k in (self._properties_from_parent
                          + self._properties_dont_copy))):
                continue
            if k in self._properties_references:
                properties_preserved[k] = self.properties[k]
            else:
                properties[k] = copy.deepcopy(
                    self.properties[k], memo)
        properties.update(properties_preserved)
        for k in self.related_units:
            if getattr(self, k, None):
                properties[k] = getattr(self, k)
        return type(self)(**properties)

    def compare(self, solf, ignore_attr=None,
                ignore_prop=None):
        if not isinstance(solf, CodeUnit):
            return False
        if ignore_attr is None:
            ignore_attr = self.parallel_units + self.related_units
        if ignore_prop is None:
            ignore_prop = (
                self._properties_dont_compare
                + self._recursive_properties)
        if not (ignore_attr or ignore_prop):
            return (self.__dict__ == solf.__dict__)
        self_dict = {k: v for k, v in self.__dict__.items()
                     if k not in ignore_attr}
        solf_dict = {k: v for k, v in solf.__dict__.items()
                     if k not in ignore_attr}
        self_dict['properties'] = {
            k: v for k, v in self_dict['properties'].items()
            if k not in ignore_prop
        }
        solf_dict['properties'] = {
            k: v for k, v in solf_dict['properties'].items()
            if k not in ignore_prop
        }
        return self_dict == solf_dict

    def __repr__(self):
        return f"{self.__class__.__name__}({self.address})"

    @classmethod
    def reset_class_properties(cls):
        for x in cls._generated_class_properties:
            setattr(cls, x, None)
        cls._generated_class_properties = []

    @classmethod
    def code_units(cls):
        return code_unit_registry(language=cls.language)

    @classmethod
    def code_unit(cls, unit_type, default=NoDefault):
        out = cls.code_units().get(unit_type, NoDefault)
        if out == NoDefault and unit_type.startswith('forward_'):
            out = cls.code_units().get(unit_type.split('forward_')[-1],
                                       NoDefault)
            if out != NoDefault and out.no_forward_unit:
                out = NoDefault
        if out != NoDefault:
            return out
        if default != NoDefault:
            return default
        raise InvalidUnitType(
            f"No unit_type \'{unit_type}\' associated "
            f"with language \'{cls.language}\'")

    def as_type(self, **kwargs):
        if self.unit_type == 'type':
            return self
        raise NotImplementedError(
            f"Cannot convert {self.language} {self.unit_type} to type")

    @classmethod
    def get_regex(cls, no_group=False, no_recurse=False, key='_regex',
                  alt_regex_fstring=None, return_fstring=False, **kwargs):
        using_default = (alt_regex_fstring is None)
        dont_cache = [(cls.optional or ('regex' in cls.dont_cache))
                      or (not using_default) or return_fstring]
        skip_sub = False
        skip_nogroup = False
        if no_recurse:
            assert alt_regex_fstring is None
            assert key == '_regex'
            key += '_norecurse'
        if no_group and using_default:
            key += '_nogroup'
            if ((not (getattr(cls, key, None)
                      or getattr(cls, key + '_fstring', None)))):
                alt_regex_fstring = cls.get_regex(
                    no_recurse=no_recurse, return_fstring=return_fstring,
                    **kwargs)
                using_default = False
                skip_sub = True
        if using_default:
            if cls.optional and kwargs.get(f'disable_{cls.unit_type}',
                                           False):
                return ''
            if ((getattr(cls, key, None) is not None
                 and not return_fstring)):
                return getattr(cls, key)
            key_fstring = key + '_fstring'
            alt_regex_fstring = getattr(cls, key_fstring, None)
            if alt_regex_fstring is None:
                msg = (
                    f"No {key} or {key_fstring} set for {cls} "
                    f"(unit_type = \'{cls.unit_type}\', "
                    f"language = \'{cls.language}\')"
                )
                print(msg)
                pdb.set_trace()
                raise NotImplementedError(msg)
        out = alt_regex_fstring
        if not (skip_sub or return_fstring):
            out = cls.regex_formatter.sub(cls, out,
                                          dont_cache=dont_cache,
                                          **kwargs)
        if no_group and not skip_nogroup:
            out = RegexFormatter.strip_names(out)
        if dont_cache[0]:
            return out
        assert out is not None
        setattr(cls, key, out)
        cls._generated_class_properties.append(key)
        return out

    def get_fstring(self, alt_fstring_cond=None, **kwargs):
        if self.properties.get('skipped', False):
            return ''
        if alt_fstring_cond is None and self._fstring_cond_prop:
            for k, v in self._fstring_cond_prop.items():
                if k in self.properties.items():
                    alt_fstring_cond = v
                    break
        if alt_fstring_cond is None:
            if self._fstring is not None:
                return self._fstring
            if self._fstring_cond is None:
                raise NotImplementedError(
                    f"No _fstring_cond set for {self.__class__} "
                    f"(unit_type = \'{self.unit_type}\', "
                    f"language = \'{self.language}\')")
            out = self._fstring_cond
        else:
            out = alt_fstring_cond
        return self.fstring_formatter.sub(self, out, **kwargs)

    @property
    def all_properties(self):
        return dict(self.properties, **self.unused_properties)

    @property
    def address_tuple(self):
        parts = [x for x in self.properties.get('unitpath', [])]
        parts.append(self.address_local)
        return tuple(parts)

    @property
    def address_local(self):
        if self.address_property is not None:
            out = self.properties[self.address_property]
            if isinstance(out, CodeUnit):
                out = out.format()
            return out
        elif 'name' in self.properties:
            return self.properties['name']
        elif ((not self.is_dummy_unit)
              and (self.unit_type not in ['template', 'preamble'])):
            print('CHECK address_local', self.__class__, self.unit_type,
                  self._properties)
            pdb.set_trace()
        return self.unit_type

    @property
    def address_stripped(self):
        sep = self.code_unit('modsep')
        return sep.join(self.address_tuple)

    @property
    def address(self):
        return self.address_stripped

    @classmethod
    def from_match(cls, match, member_units=None, check_format=False,
                   **kwargs):
        kwargs = cls.complete_match(match, kwargs,
                                    member_units=member_units,
                                    check_format=check_format)
        kwargs = dict(match.groupdict(), match=match, **kwargs)
        kwargs.setdefault('match_start', match.start())
        kwargs.setdefault('match_end', match.end())
        kwargs.setdefault(
            'match_string',
            match.string[kwargs['match_start']:kwargs['match_end']])
        kwargs = {k: cls.parse_property(k, v)
                  for k, v in kwargs.items() if v is not None}
        return cls(check_format=check_format, **kwargs)

    @classmethod
    def from_unit(cls, x, property_name=None, property_index=None,
                  generating_unit=None, dont_convert=False,
                  conversion_prefix=None, member_types=None, **kwargs):
        if dont_convert:
            if generating_unit is not None:
                kwargs['generating_unit'] = generating_unit
            kwargs = dict(x.properties, **kwargs)
            kwargs = dict(
                {k: getattr(x, k) for k in cls.parallel_units},
                **kwargs)
            return cls(**kwargs)
        if generating_unit and property_name not in [None, 'members']:
            generating_unit = None
        if generating_unit and property_index is not None:
            try:
                generating_unit = generating_unit.find_member(
                    x, key='wrapped_unit')
            except KeyError:
                print(f"Missing parallel unit {x} in "
                      f"generating_unit {generating_unit}")
                pdb.set_trace()
                raise
        if generating_unit:
            kwargs['generating_unit'] = generating_unit
        if isinstance(x, list):
            out = [
                cls.from_unit(xx, property_name=property_name,
                              conversion_prefix=conversion_prefix,
                              property_index=i, **kwargs)
                for i, xx in enumerate(x)]
            if member_types is not None and property_name == 'members':
                out = [xx for xx in out if xx.unit_type in member_types]
            return out
        elif isinstance(x, CodeUnit):
            unit = None
            if conversion_prefix:
                try:
                    unit = cls.code_unit(conversion_prefix + x.unit_type)
                except KeyError:
                    pass
            if unit is None:
                unit = cls.code_unit(x.unit_type)
            kwargs = dict(
                {k: cls.from_unit(
                    v, property_name=k,
                    parent=kwargs.get(
                        'name', x.properties.get('name', None)),
                    unitpath=kwargs.get(
                        'unitpath', []) + [
                            kwargs.get('name', x.address_local)],
                    generating_unit=generating_unit,
                    conversion_prefix=conversion_prefix,
                    member_types=member_types)
                 for k, v in x.properties.items()
                 if k not in cls._recursive_properties},
                **kwargs)
            kwargs['wrapped_unit'] = x
            out = unit(**kwargs)
            # Set base_class_unit to link as child in base class
            if 'base_class_unit' in kwargs:
                kwargs['base_class_unit'].properties[
                    'wrapped_unit'] = x.properties['base_class_unit']
                if generating_unit:
                    kwargs['base_class_unit'].generating_unit = (
                        generating_unit.properties['base_class_unit'])
                out.set_property('base_class_unit',
                                 kwargs['base_class_unit'])
            return out
        return x

    @classmethod
    def parse(cls, x, pos=None, endpos=None, return_match=False,
              member_units=None, check_format=False, regex_prefix=None,
              regex_suffix=None, contiguous=False, **kwargs):
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(x)
        regex = cls.get_regex(**kwargs)
        if regex_prefix is not None:
            regex = regex_prefix + regex
        if regex_suffix is not None:
            regex = regex + regex_suffix
        try:
            pattern = re.compile(regex, flags=re.MULTILINE)
        except BaseException:
            print(f"FAILED TO COMPILE REGEX for {cls}")
            print(regex)
            pdb.set_trace()
            raise
        if contiguous:
            match = pattern.match(x, pos, endpos)
        else:
            match = pattern.search(x, pos, endpos)
        block_comment = cls.code_unit('block_comment')
        if match and block_comment:
            if isinstance(block_comment[0], list):
                idx_begin = max([x.rfind(c, 0, match.start())
                                 for c in block_comment[0]])
            else:
                idx_begin = x.rfind(block_comment[0], 0, match.start())
            if isinstance(block_comment[1], list):
                idx_end = max([x.rfind(c, 0, match.start())
                               for c in block_comment[1]])
            else:
                idx_end = x.rfind(block_comment[1], 0, match.start())
            if idx_begin != -1 and idx_begin > idx_end:
                match = None
        if return_match:
            return match
        if match:
            return cls.from_match(match, member_units=member_units,
                                  check_format=check_format, **kwargs)

    @classmethod
    def parse_subunit(cls, x, pos=None, endpos=None, units=None,
                      cached_matches=None, **kwargs):
        if units is None:
            units = cls.member_units
        match = None
        unit = None
        ipos = pos
        iendpos = endpos
        for iunit in units:
            if isinstance(iunit, str):
                iunit = cls.code_unit(iunit)
            if ((cached_matches is not None and iunit in cached_matches
                 and cached_matches[iunit].start() > ipos)):
                imatch = cached_matches[iunit]
            else:
                imatch = iunit.parse(x, pos=ipos, endpos=iendpos,
                                     return_match=True, **kwargs)
                if imatch and cached_matches is not None:
                    cached_matches[iunit] = imatch
            if imatch and (match is None or
                           imatch.start() < match.start()):
                match = imatch
                unit = iunit
                iendpos = match.start()
        if match:
            return unit.from_match(match, **kwargs)

    @classmethod
    def parse_subunits(cls, x, pos=None, endpos=None, units=None,
                       cached_matches=None, member_index=0,
                       fullmatch=False, **kwargs):
        if pos is None:
            pos = 0
        if endpos is None:
            endpos = len(x)
        whitespace = re.compile(r'\s*')
        previous_matches = []
        if cached_matches is None:
            cached_matches = {}
        kwargs['cached_matches'] = cached_matches
        pos_final = pos
        while pos < endpos:
            assert pos >= 0
            match = cls.parse_subunit(x, pos=pos, endpos=endpos,
                                      member_index=member_index,
                                      units=units, **kwargs)
            if match:
                yield match
                pos = match.match_end
                member_index += 1
                previous_matches.append(match)
                pos_final = pos
            elif (member_index > 0 and kwargs.get('contiguous', False)):
                match = whitespace.match(x, pos=pos, endpos=endpos)
                if match:
                    pos = match.end()
                match = cls.parse_subunit(x, pos=pos, endpos=endpos,
                                          member_index=member_index,
                                          units=units, **kwargs)
                if match:
                    raise Exception(f"Match after whitespace removed: "
                                    f"{match}")
                for unit in units:
                    pattern = (
                        kwargs.get('regex_prefix', '')
                        + cls.code_unit(unit).get_regex(**kwargs)
                        + kwargs.get('regex_suffix', ''))
                    check_regex(pattern, x[pos:endpos])
                    print(pattern)
                print(previous_matches)
                print(f"pos = {pos}, endpos = {endpos}")
                print(x[pos:endpos])
                pdb.set_trace()
                raise ValueError(
                    f"Failed to find a match, but the end has not been "
                    f"reached (pos = {pos}, endpos = {endpos}). "
                    f"REMAINDER = \n{x[pos:endpos]}")
            else:
                pos_final = pos
                pos = endpos
        if ((fullmatch and pos_final < endpos
             and not x[pos_final:endpos].isspace())):
            msg = (f"Match did not fill the entire string. The "
                   f"remainder is \'{x[pos_final:endpos]}\'")
            print(msg)
            pdb.set_trace()
            raise ValueError(msg)

    @classmethod
    def complete_match_body(cls, match, pos=None, endpos=None,
                            parent_body_match=None):
        if 'body' in match.groupdict():
            if match.group('body') is None:
                return None
            return DummyMatch(match.string, start=match.start('body'),
                              end=match.end('body'))
        body_match = None
        if pos is None:
            pos = match.start()
        if endpos is None:
            endpos = match.end()
        context_token_match = None
        if cls.member_context_token:
            context_token_match = cls.member_context_token.match_a(
                match.string, pos, endpos, location='end')
        if context_token_match:
            context_tokens = cls.code_unit('context_tokens')
            kws = {'tokens': [x for x in context_tokens.values()
                              if x.exclusive]}
            if parent_body_match is not None:
                kws['endpos'] = parent_body_match.end()
            context_match = cls.member_context_token.match(
                match.string, context_token_match.start(), **kws)
            assert context_match
            body_match = context_match.body_match()
        return body_match

    @classmethod
    def complete_match(cls, match, kwargs, member_units=None,
                       check_format=False):
        if member_units is None:
            member_units = cls.member_units
        if 'members' in cls._properties + cls._properties_optional:
            assert member_units
        if member_units:
            if 'name' not in kwargs:
                kwargs['name'] = match.group('name')
            kwargs.setdefault('member_kwargs', {})
            kwargs.setdefault('recursive_member_kwargs', {})
            kwargs['member_kwargs'].setdefault(
                'parent', kwargs['name'])
            kwargs['member_kwargs'].setdefault(
                'check_format', check_format)
            kwargs['member_kwargs'].setdefault(
                'recursive_member_kwargs',
                kwargs['recursive_member_kwargs'])
            kwargs['member_kwargs'].update(
                **kwargs['recursive_member_kwargs'])
            unitpath = [x for x in kwargs.get('unitpath', [])]
            unitpath.append(kwargs['member_kwargs']['parent'])
            kwargs['member_kwargs'].setdefault('unitpath', unitpath)
            for k in kwargs.keys():
                if k.startswith('disable_'):
                    kwargs['member_kwargs'][k] = kwargs[k]
        kwargs.setdefault('match_start', match.start())
        kwargs.setdefault('match_end', match.end())
        if 'body' not in kwargs:
            body_match = cls.complete_match_body(
                match, pos=kwargs['match_start'],
                endpos=kwargs['match_end'],
                parent_body_match=kwargs.get('parent_body_match', None))
            if body_match:
                kwargs['body_match'] = body_match
                kwargs['body'] = body_match.group(0)
                kwargs['body_start'] = body_match.start()
                kwargs['body_end'] = body_match.end()
                kwargs['match_end'] = body_match.end()
                if member_units:
                    kwargs['member_kwargs'].setdefault(
                        'parent_body_match', body_match)
        if member_units and 'members' not in kwargs:
            addresses = []
            kwargs['members'] = []
            for m in cls.parse_subunits(
                    match.string,
                    pos=kwargs.get('body_start', kwargs['match_start']),
                    endpos=kwargs.get('body_end', kwargs['match_end']),
                    units=member_units,
                    **kwargs.get('member_kwargs', {})):
                if ((m.address in addresses
                     and not m.allow_duplicates)):
                    prev_idx = addresses.index(m.address)
                    prev = kwargs['members'][prev_idx]
                    macros = [x for x in kwargs['members'] if
                              x.unit_type == 'macro']
                    selected = m.check_duplicate(prev, macros, match,
                                                 kwargs)
                    if (not selected) or selected == prev:
                        continue
                    else:
                        assert selected == m
                        addresses.pop(prev_idx)
                        kwargs['members'].pop(prev_idx)
                if m.allow_duplicates != 'ignore':
                    kwargs['members'].append(m)
                    addresses.append(m.address)
        return kwargs

    def check_duplicate(self, solf, macros, parent_match, parent_kws):
        if self.unit_type != solf.unit_type:
            if self.unit_type == f'forward_{solf.unit_type}':
                return solf
            elif solf.unit_type == f'forward_{self.unit_type}':
                return self
        if 'return_macro_tokens' in parent_kws:
            tokens = parent_kws['return_macro_tokens']
            self_tokens = tokens.tokens_intersecting(
                'overlaps', self, include_tokens=['return_macro'])
            solf_tokens = tokens.tokens_intersecting(
                'overlaps', solf, include_tokens=['return_macro'])
            if self_tokens != solf_tokens:
                return solf
        msg = (f"Duplicate member located:\n"
               f"{self.address}\n"
               f"{pprint.pformat(self.properties)} vs.\n"
               f"{solf.address}\n"
               f"{pprint.pformat(solf.properties)}\n"
               f"Parent kwargs:\n{pprint.pformat(parent_kws)}")
        print(msg)
        pdb.set_trace()
        raise ValueError(msg)

    @classmethod
    def _before_registration(cls):
        pass

    @classmethod
    def add_subunits(cls):
        for p in cls._properties + cls._properties_optional:
            try:
                unit = cls.code_unit(cls.property_units.get(p, p))
                for x in ['property_subunits', 'property_units',
                          'list_seps', 'list_bounds']:
                    for k, v in getattr(unit, x).items():
                        getattr(cls, x).setdefault(k, v)
            except InvalidUnitType:
                pass

    @classmethod
    def parse_property(cls, k, x):
        if k == 'indent':
            return int(len(x) / len(cls.code_unit(k)))
        try:
            unit = cls.code_unit(cls.property_units.get(k, k))
            if not unit.circular:
                return unit.parse(x)
        except InvalidUnitType:
            pass
        if k in cls.property_subunits:
            if k not in cls.list_seps:
                print(f"SUBUNIT {k} in {cls} does not have a list "
                      f"separator. Existing list_seps = {cls.list_seps}")
                pdb.set_trace()
            sep = re.escape(cls.list_seps[k].strip())
            end = re.escape(cls.list_bounds.get(k, ('', ''))[-1].strip())
            units = cls.property_subunits[k]
            if not isinstance(units, list):
                units = [units]
            if end:
                end = r'|(?=' + end + ')'
            kws = {
                'fullmatch': True,
                'contiguous': True,
                'units': units,
                'regex_prefix': r'\s*',
                'regex_suffix': (
                    r'\s*(?:(?:' + sep + r')|(?:$)' + end + ')'),
            }
            out = list(cls.parse_subunits(x, **kws))
            for i, x in enumerate(out):
                x.properties['subunit_index'] = i
            return out
        return x

    @classmethod
    def format_property(cls, k, x, parent_result=None):
        if k == 'indent':
            assert isinstance(x, int)
            return x * cls.code_unit('indent')
        elif isinstance(x, CodeUnit):
            return x.format(parent_result=parent_result)
        elif isinstance(x, list):
            sep = '\n'
            if k in cls.list_seps:
                sep = cls.list_seps[k]
            elif k == 'unitpath':
                sep = cls.code_unit('modsep')
            if k in cls.property_subunits:
                kk = cls.property_subunits[k]
            else:
                kk = k
            vals = [cls.format_property(kk, xx) for xx in x]
            vals = [xx for xx in vals if xx]
            if sep == '\n':
                vals = [xx for xx in vals if not xx.isspace()]
            return sep.join(vals)
        return str(x)

    def format(self, **kws):
        out = self.get_fstring(**kws)
        if not out:
            return out
        indent = self.format_property('indent',
                                      self.get_property('indent'))
        out = out.replace(f'\n{self._noindent_flag}',
                          self._noindent_flag)
        out = indent + out.replace('\n', f'\n{indent}')
        out = out.replace(self._noindent_flag, '\n')
        return out

    def selected(self, member_units=None,
                 member_names=None, member_addresses=None,
                 member_types=None, required_conditions='all',
                 member_properties=None, **kwargs):
        conditions = []
        kwargs['default'] = None
        if member_units is not None:
            conditions.append(self.unit_type in member_units)
        if (member_names is not None) and ('name' in self.properties):
            conditions.append(self.properties['name'] in member_names)
        if member_addresses is not None:
            conditions.append(self.address in member_addresses)
        if member_types is not None:
            conditions.append(
                any(x in member_types for x in self.utilized_types()))
        if member_properties is not None:
            for k, v in member_properties.items():
                if k is None:
                    conditions.append(self == v)
                else:
                    conditions.append(self.get_property(k, **kwargs) == v)
        if required_conditions == 'all':
            return all(conditions)
        else:
            return any(conditions)

    def copy_members(self, solf, **kwargs):
        kwargs.setdefault('make_copy', True)
        self.add_members(solf.properties.get('members', []), **kwargs)

    def remove_members(self, members=None, **kwargs):
        new_members = []
        for x in self.properties.get('members', []):
            if not (x.selected(required_conditions='any', **kwargs)
                    or (members and x in members)):
                new_members.append(x)
        self.properties['members'] = new_members

    def select_members(self, **kwargs):
        members = self.properties.get('members', [])
        self.properties['members'] = []
        self.add_members(members, **kwargs)

    def prepend_member(self, x, **kwargs):
        kwargs['index'] = 0
        return self.add_member(x, **kwargs)

    def append_member(self, x, **kwargs):
        kwargs['index'] = -1
        return self.add_member(x, **kwargs)

    def add_container(self, unit_type, **kwargs):
        members = self.properties.pop('members', [])
        self.properties['members'] = []
        self.add_member(self.code_unit(unit_type)(**kwargs))
        self.properties['members'][-1].add_members(members)

    def add_members(self, members=None, dont_update_unitpath=False,
                    index=-1, make_copy=False, **kwargs):
        if isinstance(members, dict) and 'members' in members:
            return self.add_members(**members)
        if not isinstance(members, list):
            members = [members]
        for x in members:
            if x.selected(**kwargs):
                self.add_member(
                    x, dont_update_unitpath=dont_update_unitpath,
                    index=index, make_copy=make_copy)
                if index >= 0:
                    index += 1

    def iter_child_members(self, min_depth=-1, max_depth=-1,
                           method='depth_first'):
        if method == 'breadth_first':
            if min_depth == -1:
                min_depth = 0
            if max_depth == -1:
                max_depth = self.get_property('max_depth') + 1
            for depth in range(min_depth, max_depth):
                for x in self.iter_child_members(min_depth=depth,
                                                 max_depth=(depth + 1)):
                    yield x
        elif method == 'depth_first':
            if max_depth != 0:
                for x in self.properties.get('members', []):
                    if min_depth <= 0:
                        yield x
                    if max_depth > 0:
                        max_depth -= 1
                    if min_depth > 0:
                        min_depth -= 1
                    for xx in x.iter_child_members(min_depth=min_depth,
                                                   max_depth=max_depth):
                        yield xx
        else:
            raise ValueError("Invalid method \'{method}\'")

    def add_member(self, x, dont_update_unitpath=False,
                   index=-1, make_copy=False):
        if isinstance(x, dict):
            y = self.code_unit(x.pop('unit_type'))(**x)
        elif make_copy:
            y = copy.deepcopy(x)
            if y.base_unit is None:
                y.base_unit = x
        else:
            y = x
        y.set_property('parent', self.address_local)
        y.set_property('indent', self.child_indent)
        y.parent_unit = self
        if not dont_update_unitpath:
            y.properties['unitpath'] = [
                x for x in
                self.properties.get('unitpath', [])
            ]
            y.properties['unitpath'].append(y.properties['parent'])
        self.properties.setdefault('members', [])
        if index == -1:
            y.properties['member_index'] = len(
                self.properties['members'])
            self.properties['members'].append(y)
        else:
            self.properties['members'].insert(index, y)
            for i, m in enumerate(self.properties['members']):
                m.properties['member_index'] = i
        self.transfer_properties_for_parent(
            'members', y, index=index,
            dont_update_unitpath=dont_update_unitpath)
        base_class = y.properties.get('base_class_unit', None)
        if base_class:
            x = y.find_sibling(key='name', allow_multiple=False,
                               default=None,
                               value=base_class.properties['name'])
            if x and x != base_class:
                y.set_property('base_class_unit', x)

    def append_properties_for_parent(self, src, dst='new', key=None,
                                     skip_keys=[], **kwargs):
        for_self = False
        if dst == 'properties':
            for_self = True
            dst = self.properties
        elif dst == 'used':
            self.properties.setdefault(
                'properties_for_parent_used', {})
            dst = self.properties['properties_for_parent_used']
        elif dst == 'new':
            self.properties.setdefault(
                'properties_for_parent', {})
            if key == NoDefault:
                dst = self.properties['properties_for_parent']
            else:
                self.properties['properties_for_parent'].setdefault(key, {})
                dst = self.properties['properties_for_parent'][key]
        elif not isinstance(dst, dict):
            raise NotImplementedError(f"Unsupport dst \'{dst}\'")
        for k, v in src.items():
            idx = -1
            if for_self and isinstance(k, tuple):
                idx = k[1]
                k = k[0]
            if k in skip_keys:
                continue
            if for_self and k == 'members':
                for vv in v:
                    if not self.has_member(vv):
                        self.add_member(vv, index=idx, **kwargs)
            elif isinstance(v, list):
                dst.setdefault(k, [])
                if idx == -1:
                    dst[k] += v
                else:
                    assert idx <= len(dst[k])
                    dst[k] = dst[k][:idx] + v + dst[k][idx:]
            elif isinstance(v, dict):
                dst.setdefault(k, {})
                self.append_properties_for_parent(v, dst=dst[k])
            else:
                if k in dst:
                    raise AssertionError(f"{k} already in destination:\n"
                                         f"{pprint.pprint(dst)}")
                dst[k] = v

    def transfer_properties_for_parent(self, key, child, idx=None,
                                       **kwargs):
        if isinstance(child, list):
            for i, y in enumerate(child):
                self.transfer_properties_for_parent(
                    key, y, idx=i, **kwargs)
            return
        if not isinstance(child, CodeUnit):
            return
        if 'properties_for_parent' not in child.properties:
            return
        new_properties = {}
        for k in [None, self.unit_type, (self.unit_type,
                                         self.address_local)]:
            if k not in child.properties['properties_for_parent']:
                continue
            self.append_properties_for_parent(
                child.properties['properties_for_parent'].pop(k),
                dst=new_properties)
        if new_properties:
            self.append_properties_for_parent(
                new_properties, dst='properties')
            child.append_properties_for_parent(
                new_properties, dst='used')
        all_properties = child.properties.pop('properties_for_parent')
        if all_properties:
            self.append_properties_for_parent(
                all_properties, key=NoDefault)
            child.append_properties_for_parent(
                all_properties, dst='used')

    def has_member(self, value, **kwargs):
        kwargs['default'] = None
        return (self.find_member(value, **kwargs) is not None)

    def find_siblings(self, *args, **kwargs):
        kwargs['allow_multiple'] = True
        return self.find_sibling(*args, **kwargs)

    def find_sibling(self, key=NoDefault, value=NoDefault, **kwargs):
        if key == NoDefault:
            key = 'name'
        if value == NoDefault:
            if key is None:
                value = self
            else:
                value = self.get_property(key)
        return self.parent_unit.find_member(value, key=key, **kwargs)

    def find_parents(self, *args, **kwargs):
        kwargs['allow_multiple'] = True
        return self.find_parent(*args, **kwargs)

    def find_parent(self, default=NoDefault, matches=None,
                    allow_multiple=False, **kwargs):
        first_call = (matches is None)
        if matches is None:
            matches = []
        if self.parent_unit:
            if self.parent_unit.selected(**kwargs):
                matches.append(self.parent_unit)
                if not allow_multiple:
                    return self.parent_unit
            self.parent_unit.find_parent(matches=matches,
                                         allow_multiple=allow_multiple,
                                         **kwargs)
        if matches or (not first_call):
            if (not allow_multiple) and matches:
                if len(matches) > 1:
                    print(matches)
                assert len(matches) == 1
                return matches[0]
            return matches
        if default != NoDefault:
            return default
        raise KeyError(f"Could not find parent that matches:\n"
                       f"{pprint.pformat(kwargs)}")

    def find_members(self, value=NoDefault, **kwargs):
        kwargs['allow_multiple'] = True
        return self.find_member(value, **kwargs)

    def find_member(self, value=NoDefault, key=NoDefault,
                    return_index=False, default=NoDefault,
                    allow_multiple=False, **kwargs):
        kwargs['default'] = None
        if value != NoDefault:
            if key == NoDefault:
                if isinstance(value, CodeUnit):
                    key = None
                else:
                    assert isinstance(value, str)
                    key = 'name'
            kwargs.setdefault('member_properties', {})
            assert key not in kwargs['member_properties']
            kwargs['member_properties'][key] = value
        out = []
        for i, x in enumerate(self.properties.get('members', [])):
            if x.selected(**kwargs):
                if allow_multiple:
                    if return_index:
                        out.append(i)
                    else:
                        out.append(x)
                else:
                    if return_index:
                        return i
                    return x
        if allow_multiple and out:
            return out
        if default != NoDefault:
            return default
        raise KeyError(f"Could not find member that matches:\n"
                       f"{pprint.pformat(kwargs)}")

    def __getitem__(self, k):
        return self.find_member(k)

    def __hash__(self):
        return hash(self.address)

    def index(self, x, key=NoDefault):
        return self.find_member(x, key=key, return_index=True)

    def check_get_property_escape(self, k, default=NoDefault,
                                  from_unit=None, **kwargs):
        if from_unit in self.related_units:
            return True
        if k in kwargs:
            return True
        return False

    def get_property(self, k, default=NoDefault, from_unit=None,
                     **kwargs):
        if from_unit in self.related_units:
            return self.get_property(from_unit).get_property(
                k, default=default, **kwargs)
        if k in kwargs:
            return kwargs[k]
        if k == 'unit_type':
            return self.unit_type
        elif k == 'address':
            return self.address
        elif k == 'address_stripped':
            return self.address_stripped
        elif k == "NOINDENT":
            return self._noindent_flag
        elif k == 'fullname':
            parts = (
                self.properties['unitpath'][1:]
                + [self.get_property('name')])
            return self.code_unit('modsep').join(parts)
        elif (k in ['base_class', 'child_class']
              and k not in self.properties
              and f'{k}_unit' in self.properties):
            return self.get_property(f'{k}_unit').get_property('name')
        elif k in ['members']:
            default = []
        elif k == 'parent_match':
            return self.parent_unit.match
        elif k in self.parallel_units:
            return self.get_parallel_unit(k, default=default)
        elif k in self.related_units:
            if getattr(self, k) is not None:
                return getattr(self, k)
            if k in ['file_unit']:
                out = None
                if self.unit_type == 'file':
                    out = self
                elif self.parent_unit:
                    out = self.parent_unit.get_property(k, None)
                if out:
                    setattr(self, k, out)
                    return out
            if default != NoDefault:
                return default
            raise KeyError(f"{self.language} {self.unit_type} does not "
                           f"have related unit \'{k}\'")
        if k in self.properties:
            return self.properties[k]
        if k == 'forward_unit':
            out = self.from_unit(self,
                                 # generating_unit=self.generating_unit,
                                 conversion_prefix='forward_')
            self.set_property('forward_unit', out)
            return out
        elif k in 'type' and self.unit_type == 'class':
            return self.as_type(**kwargs)
        if default != NoDefault:
            return default
        raise KeyError(f"{self.language} {self.unit_type} does not "
                       f"have property \'{k}\'")

    def get_parallel_unit(self, k, default=NoDefault):
        if getattr(self, k, None) is not None:
            return getattr(self, k)
        if self.parent_unit is None:
            if default != NoDefault:
                return default
            try:
                raise KeyError(f"Failed to find parent with \'{k}\' set")
            except BaseException:
                pdb.set_trace()
                raise
        idx = self.parent_unit.index(self)
        try:
            out = self.parent_unit.get_parallel_unit(k).get_property(
                'members')[idx]
        except KeyError:
            if default != NoDefault:
                return default
            raise
        setattr(self, k, out)
        return out

    def set_property(self, k, v):
        if k not in ['members'] + self.related_units:
            self.properties[k] = v
        if k == 'name':
            for x in self.properties.get('members', []):
                x.set_property('parent', v)
        elif k == 'members':
            self.add_members(v)
        elif k == 'unitpath':
            prev = self.properties['unitpath'] + [self.address_local]
            for x in self.properties.get('members', []):
                x.set_property('unitpath', [x for x in prev])
        elif k == 'indent':
            for x in self.properties.get('members', []):
                x.set_property('indent', self.child_indent)
        elif k == 'base_class_unit':
            self.properties.pop('base_class', None)
            child_class = self.properties[k].get_property(
                'child_class_unit', default=[])
            if self not in child_class:
                child_class.append(self)
            self.properties[k].set_property(
                'child_class_unit', child_class)
        elif k == 'child_class_unit':
            self.properties.pop('child_class', None)
        elif k in self.parallel_units:
            self.set_parallel_unit(k, v)
        elif k in self.related_units:
            setattr(self, k, v)
            if k in ['file_unit']:
                for x in self.properties.get('members', []):
                    x.set_property(k, v)
        if (((k == self.address_property)
             or (self.address_property is None and k == 'name'))):
            self.set_property('unitpath', self.properties['unitpath'])

    def set_parallel_unit(self, k, solf):
        print('set_parallel_unit', k, self, solf)
        pdb.set_trace()
        setattr(self, k, solf)
        self_members = self.properties.get('members', [])
        solf_members = solf.properties.get('members', [])
        assert len(self_members) == len(solf_members)
        for x_self, x_solf in zip(self_members, solf_members):
            x_self.set_parallel_unit(k, x_solf)

    def call_method_for_all_children(self, method, args, kwargs,
                                     accumulate=None):
        out = []
        prev_new_siblings = kwargs.pop('new_siblings', {})

        def do_call(v):
            nonlocal out
            if isinstance(v, CodeUnit):
                iout = getattr(v, method)(*args, **kwargs)
                if accumulate:
                    out = accumulate(out, iout)
            elif isinstance(v, list):
                for vv in v:
                    out = do_call(vv)
            return out

        call_history = kwargs.pop('call_history', [])
        for k, v in self.properties.items():
            if k in self._recursive_properties:
                continue
            if k == 'members':
                kwargs['new_siblings'] = {}
            kwargs['call_history'] = call_history + [self]
            out = do_call(v)
            if k == 'members':
                for kk, vv in kwargs.pop('new_siblings', {}).items():
                    index = self.index(kk)
                    self.add_members(vv, index=index)
        if prev_new_siblings is not None:
            kwargs['new_siblings'] = prev_new_siblings
        if accumulate:
            return out

    def utilized_types(self, *args, **kwargs):
        out = [
            x.format() for x in self.utilized_type_units(*args, **kwargs)
        ]
        return out

    def utilized_type_units(self, *args, **kwargs):
        out = []
        if self.unit_type == 'type':
            out.append(self)
        elif self.unit_type in ['class']:
            out.append(self.as_type())
        out += self.call_method_for_all_children(
            'utilized_type_units', args, kwargs,
            accumulate=lambda x, y: x + y)
        return out

    def replace_type(self, a, b, **kwargs):
        self.call_method_for_all_children(
            'replace_type', (a, b), kwargs)

    def replace_types(self, types, **kwargs):
        for k, v in types.items():
            self.replace_type(k, v, **kwargs)

    def specialize(self, *args, type_replacements=None, **kwargs):
        top_level = False
        if type_replacements is None:
            type_replacements = {}
            top_level = True
        kwargs['type_replacements'] = type_replacements
        self.call_method_for_all_children(
            'specialize', args, kwargs)
        if top_level:
            for k, v in type_replacements.items():
                self.replace_type(k, v)

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


class EnumValueUnit(CodeUnit):

    unit_type = 'enum_value'
    _properties = CodeUnit._properties + ['parent']


class EnumUnit(CodeUnit):

    unit_type = 'enum'
    member_units = ['enum_value']
    _properties = CodeUnit._properties + ['members']


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
    _properties_optional = CodeUnit._properties_optional + [
        'call_at_exit'
    ]


class MethodUnit(FunctionUnit):

    unit_type = 'method'
    _properties = FunctionUnit._properties + [
        'parent',
    ]


class OperatorUnit(MethodUnit):

    unit_type = 'operator'


class ConstructorUnit(MethodUnit):

    unit_type = 'constructor'
    _properties = ['parent', 'args']
    address_property = 'parent'


class DestructorUnit(MethodUnit):

    unit_type = 'destructor'
    address_property = 'parent'


class TypeConversionUnit(MethodUnit):

    unit_type = 'type_conversion'


class ClassUnit(CodeUnit):

    unit_type = 'class'
    member_units = ['method']
    _properties = [
        'name',
    ]
    _properties_optional = CodeUnit._properties_optional + [
        'type_constructors',
    ]

    def as_type(self, **kwargs):
        unit = self.code_unit('type')
        kwargs.setdefault('name', self.properties['name'])
        # kwargs.setdefault('class_unit', self)
        return unit(**kwargs)


class ModuleUnit(CodeUnit):

    unit_type = 'module'
    member_units = ['module', 'class', 'function']
    _properties = [
        'name',
    ]


class ImportUnit(CodeUnit):

    unit_type = 'import'
    _properties = [
        'name',
    ]


class FileUnit(CodeUnit):

    unit_type = 'file'
    member_units = ['module', 'class', 'function']
    _properties = [
        'name',
    ]
    ext = []
    comment = None
    comment_token = None
    divider_char = '='
    block_comment = None
    context_tokens = {}
    ignore_blocks = []
    indent = ''
    modsep = '.'
    _fstring_cond = (
        '{members}\n'
    )
    child_indent = 0

    def __init__(self, *args, **kwargs):
        super(FileUnit, self).__init__(*args, **kwargs)
        if self.wrapped_unit:
            assert (self.properties['name']
                    != self.wrapped_unit.properties['name'])
        if not kwargs.get('dont_register', False):
            register_file_unit(self)

    @classmethod
    def reset_class_properties(cls):
        super(FileUnit, cls).reset_class_properties()
        for v in code_unit_registry(cls.language).values():
            if ((isinstance(v, type) and issubclass(v, CodeUnit)
                 and not issubclass(v, FileUnit))):
                v.reset_class_properties()

    @classmethod
    def preprocess(cls, x):
        return x, {}

    @classmethod
    def parse(cls, x, pos=None, endpos=None, return_match=False,
              member_units=None, check_format=False,
              preprocess_kws=None, output_preprocess=None,
              **kwargs):
        if preprocess_kws is None:
            preprocess_kws = {}
        kwargs.setdefault('name', 'dummy')
        assert pos is None and endpos is None
        x, kws = cls.preprocess(x, **preprocess_kws)
        if output_preprocess:
            if not os.path.isabs(output_preprocess):
                output_preprocess = os.path.join(os.getcwd(),
                                                 output_preprocess)
            with open(output_preprocess, 'w') as fd:
                fd.write(x)
            print(f"OUTPUT PREPROCESSED {x} TO {output_preprocess}")
        match = DummyMatch(x, start=pos, end=endpos)
        if return_match:
            return match
        if match:
            kwargs.update(kws)
            return cls.from_match(match, member_units=member_units,
                                  check_format=check_format, **kwargs)
