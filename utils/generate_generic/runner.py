import os
import argparse


if __name__ == "__main__":
    from generate_generic import GeneratedFile, generate
    parser = argparse.ArgumentParser(
        "Generate interfaces for rapidjson::Document in C & Fortran")
    parser.add_argument("--debug", action="store_true",
                        help="Dont't actually write out to files")
    parser.add_argument("--language", type=str,
                        help="Language to generate")
    args = parser.parse_args()
    if args.language == 'julia':
        fcpp = GeneratedFile(
            os.path.join('cpp', 'include',
                         'communicators', 'CommBase.hpp'))
        fcpp.test_parse_wrap()
    else:
        generate(debug=args.debug)
