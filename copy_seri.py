import os
import shutil

seri_dir = os.path.expanduser(
    os.path.join('~', 'yggdrasil', 'yggdrasil', 'languages', 'C'))
files = [
    {'path': os.path.join('datatypes', 'serialization.h'),
     'cpp': (
         "// Import arrays once\n"
         "// #define RAPIDJSON_FORCE_IMPORT_ARRAY\n"
         "#include \"serialization.hpp\"\n"
         "void** rapidjson_ARRAY_API = NULL;\n"),
     'replacements': [
         ('#include "../regex/regex_win32.h"',
          '#include "regex.hpp"'),
         ('#include "../regex/regex_posix.h"',
          '#include "regex.hpp"'),
         ('#include "../constants.h"',
          '#include "constants.hpp"'),
         ('#include "utils.h"', ''),
         ('ygglog_error(', 'ygglog_error_c('),
         ('ygglog_debug(', 'ygglog_debug_c('),
         ('ygglog_info(', 'ygglog_info_c('),
         ('ygglog_throw_error(', 'ygglog_throw_error_c('),
         ('find_match(', 'find_match_c('),
         ('regex_replace_sub(', 'regex_replace_c('),
         ('#define RAPIDJSON_YGGDRASIL\n', ''),
     ],
     'insert': [
         ('/*!',  # '\nclass Header {',
          '\nusing namespace communication::utils;\n'),
         ('#define STRLEN_RJ(var)',
          '#include "logging.hpp"\n\n'),
     ]},
    {'path': 'constants.h'}
]


local_dir = os.path.join(os.path.dirname(__file__),
                         'communication', 'utils')
for f in files:
    src = os.path.join(seri_dir, f['path'])
    dst = os.path.join(local_dir, os.path.basename(src))
    dst = dst.replace('.h', '.hpp')
    dst_cpp = dst.replace('.hpp', '.cpp')
    contents = open(src, 'r').read()
    for a, b in f.get('replacements', []):
        contents = contents.replace(a, b)
    for a, b in f.get('insert', []):
        idx = contents.index(a)
        contents = contents[:idx] + b + contents[idx:]
    with open(dst, 'w') as fd:
        fd.write(contents)
    with open(dst_cpp, 'w') as fd:
        fd.write(f.get('cpp', ''))
