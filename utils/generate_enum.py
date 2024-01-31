import os
import pprint
import argparse


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
no_map = []
no_map_item = {}
no_fortran = [
    'CLEANUP_MODE',
    'HEAD_RESET_MODE',
    'SIGNON_STATUS',
    'THREAD_STATUS',
    'FORK_TYPE',
]
no_fortran_item = {
    'COMM_FLAG': ['COMM_FLAG_MAX'],
}
types_cxx = {
    'COMM_FLAG': 'int64_t',
    'HeadFlags': 'int',
}


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
    lastval = None
    i = 0
    while i < len(lines):
        if k is not None:
            if '}' in lines[i]:
                k = None
                kprefix = None
                ksuffix = None
                klower = False
                kreplacement = None
                lastval = None
            else:
                member = {'doc': '', 'val': lastval + 1}
                rem = lines[i].split('//', 1)
                if len(rem) == 2:
                    member['doc'] = rem[1].strip().rstrip('!<')
                rem = rem[0].split(',')[0]
                rem = rem.split("=", 1)
                if len(rem) == 2:
                    member['explicit_val'] = True
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
                    lastval = member['val']
        elif lines[i].strip().startswith('enum'):
            k = lines[i].split('enum', 1)[1].split()[0].strip()
            kprefix = prefixes.get(k, [])
            ksuffix = suffixes.get(k, [])
            klower = lowers.get(k, False)
            kreplacement = replacements.get(k, {})
            lastval = -1
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
        if x['name'] in no_map_item.get(name, []):
            continue
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
        if k in no_map:
            continue
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
        tname = types_cxx.get(name, 'int')
        if tname == 'int':
            continue
        lines.append('')
        for x in members:
            if x['name'] in no_fortran_item.get(name, []):
                continue
            lines.append(f"  FYGG_API extern const {tname} {x['name']}_F;")
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
        tname = types_cxx.get(name, 'int')
        if tname == 'int':
            continue
        lines.append('')
        for x in members:
            if x['name'] in no_fortran_item.get(name, []):
                continue
            lines.append(
                f"  const {tname} {x['name']}_F = {x['name']};")
        lines.append('')
    lines += [
        '',
        '#ifdef __cplusplus',
        '}',
        '#endif',
    ]
    do_write(dst, lines)


# Version that binds to constants from C
def generate_fortran_c(enums, dst=None):
    if dst is None:
        dst = os.path.join(os.path.dirname(os.path.dirname(__file__)),
                           'fortran', 'YggInterface_enums.F90')
    lines = [
        '#ifndef DOXYGEN_SHOULD_SKIP_THIS',
    ]
    for name, members in enums.items():
        lines.append('')
        tname = types_cxx.get(name, 'int')
        if tname != 'int':
            for x in members:
                if x['name'] in no_fortran_item.get(name, []):
                    continue
                lines.append(
                    f"  integer(kind=c_{tname}), protected, "
                    f"bind(c, name=\"{x['name']}_F\") :: {x['name']}")
        else:
            lines += [
                '  enum, bind( C )',
                '     enumerator :: &',
            ]
            for x in members:
                if x['name'] in no_fortran_item.get(name, []):
                    continue
                lines.append(f"        {x['name']} = {x['val']}, &")
            lines[-1] = lines[-1].split(',')[0]
            lines += [
                '  end enum',
            ]
        lines.append('')
    lines += [
        '#endif'
    ]
    do_write(dst, lines)


# Version that just sets the values directly
def generate_fortran(enums, dst=None):
    if dst is None:
        dst = os.path.join(os.path.dirname(os.path.dirname(__file__)),
                           'fortran', 'YggInterface_enums.F90')
    lines = [
        '#ifndef DOXYGEN_SHOULD_SKIP_THIS',
    ]
    for name, members in enums.items():
        lines.append('')
        tname = types_cxx.get(name, 'int')
        if tname != 'int':
            tsuffix = tname.split('_')[0]
            for x in members:
                if x['name'] in no_fortran_item.get(name, []):
                    continue
                lines.append(
                    f"  integer(kind={tsuffix}), parameter :: "
                    f"{x['name']} = {x['val']}_{tsuffix}")
        else:
            lines += [
                '  enum, bind( C )',
                '     enumerator :: &',
            ]
            for x in members:
                if x['name'] in no_fortran_item.get(name, []):
                    continue
                lines.append(f"        {x['name']} = {x['val']}, &")
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
             dst_fortran_c_header=None, dst_fortran_c_src=None,
             fortran_wrap_c_enums=False):
    enums = parse(src=src)
    generate_maps(enums, dst=dst_maps)
    if fortran_wrap_c_enums:
        generate_fortran_c_header(enums, dst=dst_fortran_c_header)
        generate_fortran_c_src(enums, dst=dst_fortran_c_src)
        generate_fortran_c(enums, dst=dst_fortran)
    else:
        generate_fortran(enums, dst=dst_fortran)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        "Generate enum source code & header files based on "
        "the values defined in cpp/include/utils/enums.hpp")
    parser.add_argument("--fortran-wrap-c-enums",
                        action="store_true",
                        help="Wrap enums for fortran in a C layer")
    args = parser.parse_args()
    generate(fortran_wrap_c_enums=args.fortran_wrap_c_enums)
