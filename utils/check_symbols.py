import pprint
import argparse
import subprocess


def create_symbols(fname, undefined=False):
    args = ['nm']
    if undefined:
        args.append('-u')
    args.append(fname)
    return subprocess.check_output(args)


def find_missing(exe, lib):
    out = {'exe': [], 'lib': []}
    exe_missing = create_symbols(exe, undefined=True).splitlines()
    lib_missing = create_symbols(lib, undefined=True).splitlines()
    libsym = create_symbols(lib)
    for x in exe_missing:
        if x in lib_missing:
            out['lib'].append(x)
        elif x not in libsym:
            out['exe'].append(x)
    return out


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        "Determine missing symbols not provided by library")
    parser.add_argument("exe", type=str,
                        help="Path to executable that should be examined")
    parser.add_argument("lib", type=str,
                        help="Path to library to include")
    args = parser.parse_args()
    pprint.pprint(find_missing(args.exe, args.lib))
