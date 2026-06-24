import os
import argparse


interface_files = {
    'cpp': os.path.join('cpp', 'include', 'communicators', 'CommBase.hpp'),
}
file_generation_order = {}
interface_files_added = {}


if __name__ == "__main__":
    from generate_generic import generate
    from generate_generic.interface import get_interface_file
    parser = argparse.ArgumentParser(
        "Generate interfaces for yggdrasil_rapidjson::Document in C & Fortran")
    parser.add_argument("--debug", action="store_true",
                        help="Dont't actually write out to files")
    parser.add_argument("--language", type=str,
                        help="Language to generate")
    parser.add_argument("--verbose", action="store_true",
                        help="Display information during parsing/generation")
    parser.add_argument("--wrap-yggdrasil-rapidjson", action="store_true",
                        help=("Create an interface that uses the wrapped "
                              "yggdrasil_rapidjson api"))
    parser.add_argument("--yggdrasil-rapidjson-include-dirs", type=str,
                        help="Path to yggdrasil_rapidjson include")
    parser.add_argument("--fortran-target", type=str,
                        help="Name of the Fortran library")
    args = parser.parse_args()
    if args.language:
        x = get_interface_file(args.language)(
            wrap_yggdrasil_rapidjson=args.wrap_yggdrasil_rapidjson,
            yggdrasil_rapidjson_include_dirs=(
                args.yggdrasil_rapidjson_include_dirs),
        )
        x.generate(debug=args.debug, verbose=args.verbose,
                   cliargs=args)
    else:
        generate(debug=args.debug, verbose=args.verbose, cliargs=args)
