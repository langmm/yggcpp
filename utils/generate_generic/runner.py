import os
import argparse


interface_files = {
    'cpp': os.path.join('cpp', 'include', 'communicators', 'CommBase.hpp'),
    'julia_cxxwrap': os.path.join('julia', 'YggInterface_julia.cpp'),
    'julia': os.path.join('julia', 'YggInterface.jl'),
}
file_generation_order = {
    'cpp': ['julia_cxxwrap', 'julia'],
}
interface_files_added = {
    'julia': ['julia_cxxwrap']
}


if __name__ == "__main__":
    from generate_generic import generate
    from generate_generic.interface import JuliaInterface
    parser = argparse.ArgumentParser(
        "Generate interfaces for rapidjson::Document in C & Fortran")
    parser.add_argument("--debug", action="store_true",
                        help="Dont't actually write out to files")
    parser.add_argument("--language", type=str,
                        help="Language to generate")
    parser.add_argument("--verbose", action="store_true",
                        help="Display information during parsing/generation")
    parser.add_argument("--wrap-rapidjson", action="store_true",
                        help=("Create an interface that uses the wrapped "
                              "rapidjson api"))
    parser.add_argument("--rapidjson-include-dirs", type=str,
                        help="Path to rapidjson include")
    args = parser.parse_args()
    if args.language == 'julia':
        x = JuliaInterface()
        x.generate(debug=args.debug, verbose=args.verbose,
                   wrap_rapidjson=args.wrap_rapidjson,
                   rapidjson_include_dirs=args.rapidjson_include_dirs)
    else:
        generate(debug=args.debug)
