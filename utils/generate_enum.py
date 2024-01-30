import os
import pprint


prefixes = {
    'COMM_FLAG': ['COMM_FLAG_'],
}
suffixes = {
    'COMM_TYPE': ['_COMM'],
    'LANGUAGE': ['_LANGUAGE'],
}
lowers = {
    'LANGUAGE': True,
}
replacements = {
    'LANGUAGE': {'no': ''},
}
requires_int64 = {
    'COMM_FLAG': True,
}
no_fortran = [
    'CLEANUP_MODE',
    'HEAD_RESET_MODE',
    'SIGNON_STATUS',
    'THREAD_STATUS',
    'FORK_TYPE',
]


def do_write(dst, lines):
    contents = '\n'.join(lines) + '\n'
    print(f"{dst}\n----------------\n{contents}")
    with open(dst, 'w') as fd:
        fd.write(contents)


def parse(src=None):
    if src is None:
        src = os.path.join(os.path.dirname(os.path.dirname(__file__)),
                           'cpp', 'include', 'utils', 'enums.hpp')
    with open(src, 'r') as fd:
        lines = fd.readlines()
    out = {}
    k = None
    kprefix = None
    ksuffix = None
    klower = False
    kreplacement = None
    i = 0
    while i < len(lines):
        if k is not None:
            if '}' in lines[i]:
                k = None
                kprefix = None
                ksuffix = None
                klower = False
                kreplacement = None
            else:
                member = {'doc': '', 'val': len(out[k])}
                rem = lines[i].split('//', 1)
                if len(rem) == 2:
                    member['doc'] = rem[1].strip().rstrip('!<')
                rem = rem[0].split(',')[0]
                rem = rem.split("=", 1)
                if len(rem) == 2:
                    member['val'] = eval(rem[1].strip().rstrip('LL'))
                member['name'] = rem[0].strip()
                member['abbr'] = member['name']
                for x in kprefix:
                    member['abbr'] = member['abbr'].split(x)[-1]
                for x in ksuffix:
                    member['abbr'] = member['abbr'].split(x)[0]
                if klower:
                    member['abbr'] = member['abbr'].lower()
                member['abbr'] = kreplacement.get(member['abbr'],
                                                  member['abbr'])
                if member['name']:
                    out[k].append(member)
        elif lines[i].strip().startswith('enum'):
            k = lines[i].split('enum', 1)[1].split()[0].strip()
            kprefix = prefixes.get(k, [])
            ksuffix = suffixes.get(k, [])
            klower = lowers.get(k, False)
            kreplacement = replacements.get(k, {})
            out.setdefault(k, [])
            while '{' not in lines[i]:
                i += 1
        i += 1
    pprint.pprint(out)
    return out


def generate_map(name, members, tname=None):
    if tname is None:
        tname = name
    lines = [
        "",
        f"const std::map<const {tname}, const std::string> {name}_map {{"
    ]
    width = len(max(members, key=lambda x: len(x['name']))['name'])
    width_abbr = len(max(members, key=lambda x: len(x['abbr']))['abbr'])
    for x in members:
        pad = (width_abbr - len(x['abbr'])) * ' '
        lines.append(f"  {{{x['name']:{width}}, \"{x['abbr']}\"{pad}}},")
    lines += ["};", ""]
    if name == 'COMM_TYPE':
        alt_members = []
        for x in members:
            abbr = x['abbr']
            if abbr not in ['IPC', 'ZMQ', 'MPI', 'RMQ', 'REST']:
                abbr = abbr.title()
            alt_members.append(dict(x, abbr=(abbr + 'Comm')))
        lines += generate_map(name + '_cls', alt_members, tname=name)
    elif name == 'COMM_FLAG':
        for sub in ['FILE_FLAG']:
            alt_members = [dict(x, abbr=x['abbr'].split(sub + '_')[-1])
                           for x in members
                           if x['name'].startswith(sub)]
            lines += generate_map(sub, alt_members, tname=name)
    return lines


def generate_maps(enums, dst=None):
    if dst is None:
        dst = os.path.join(os.path.dirname(os.path.dirname(__file__)),
                           'cpp', 'include', 'utils', 'enums_maps.hpp')
    lines = [
        '#pragma once',
        '',
        '#include <map>',
        '#include <string>',
        '#include "utils/enums.hpp"',
        '',
        'namespace YggInterface {',
        '  namespace utils {',
    ]
    for k, v in enums.items():
        lines += ['    ' + x for x in generate_map(k, v)]
    lines += [
        '  }',
        '}',
    ]
    do_write(dst, lines)


def generate_fortran_c_header(enums, dst=None):
    if dst is None:
        dst = os.path.join(os.path.dirname(os.path.dirname(__file__)),
                           'fortran', 'c_wrappers_enums.h')
    lines = [
        '#ifndef YGG_FC_ENUM_WRAPPERS_H_',
        '#define YGG_FC_ENUM_WRAPPERS_H_',
        '',
        '#ifndef DOXYGEN_SHOULD_SKIP_THIS',
        '',
        '#include "YggInterface_fortran_export.h"',
        '',
        '#ifdef __cplusplus /* If this is a C++ compiler, use C linkage */',
        '#include <cstdint>',
        'extern "C" {',
        '#else',
        '#include "stdint.h"',
        '#endif',
        ''
    ]
    for name, members in enums.items():
        if not requires_int64.get(name, False):
            continue
        lines.append('')
        for x in members:
            lines.append(f"  extern FYGG_API int64_t {x['name']}_F;")
        lines.append('')
    lines += [
        '',
        '#ifdef __cplusplus',
        '}',
        '#endif',
        '',
        '#endif // DOXYGEN_SHOULD_SKIP_THIS',
        '',
        '#endif // YGG_FC_ENUM_WRAPPERS_H_'
    ]
    do_write(dst, lines)


def generate_fortran_c_src(enums, dst=None):
    if dst is None:
        dst = os.path.join(os.path.dirname(os.path.dirname(__file__)),
                           'fortran', 'c_wrappers_enums.c')
    lines = [
        '#include "c_wrappers_enums.h"',
        '#include "utils/enums.hpp"',
        '',
        '#ifdef __cplusplus /* If this is a C++ compiler, use C linkage */',
        'extern "C" {',
        '#endif',
        ''
    ]
    for name, members in enums.items():
        if not requires_int64.get(name, False):
            continue
        lines.append('')
        for x in members:
            lines.append(
                f"  int64_t {x['name']}_F = {x['name']};")
        lines.append('')
    lines += [
        '',
        '#ifdef __cplusplus',
        '}',
        '#endif',
    ]
    do_write(dst, lines)


def generate_fortran(enums, dst=None):
    if dst is None:
        dst = os.path.join(os.path.dirname(os.path.dirname(__file__)),
                           'fortran', 'YggInterface_enums.F90')
    lines = [
        '#ifndef DOXYGEN_SHOULD_SKIP_THIS',
    ]
    for name, members in enums.items():
        lines.append('')
        if requires_int64.get(name, False):
            for x in members:
                lines.append(
                    f"  integer(kind=c_int64_t), protected, "
                    f"bind(c, name=\"{x['name']}_F\") :: {x['name']}")
        else:
            lines += [
                '  enum, bind( C )',
                '     enumerator :: &',
            ]
            for x in members:
                lines.append(f"        {x['name']}, &")
            lines[-1] = lines[-1].split(',')[0]
            lines += [
                '  end enum',
            ]
        lines.append('')
    lines += [
        '#endif'
    ]
    do_write(dst, lines)


def generate(src=None, dst_maps=None, dst_fortran=None,
             dst_fortran_c_header=None, dst_fortran_c_src=None):
    enums = parse(src=src)
    generate_maps(enums, dst=dst_maps)
    generate_fortran_c_header(enums, dst=dst_fortran_c_header)
    generate_fortran_c_src(enums, dst=dst_fortran_c_src)
    generate_fortran(enums, dst_fortran)


if __name__ == "__main__":
    generate()
