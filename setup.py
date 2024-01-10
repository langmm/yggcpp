# -*- coding: utf-8 -*-
# :Project:   python-rapidjson -- Packaging
# :Author:    Ken Robbins <ken@kenrobbins.com>
# :License:   MIT License
# :Copyright: © 2015 Ken Robbins
# :Copyright: © 2016, 2017, 2018, 2019, 2020, 2021, 2022 Lele Gaifax
#

import os.path
import sys
from skbuild import setup


if sys.version_info < (3, 6):
    raise NotImplementedError("Only Python 3.6+ is supported.")


ROOT_PATH = os.path.abspath(os.path.dirname(__file__))
rj_include_dir = os.path.join(ROOT_PATH, 'cpp',
                              'rapidjson', 'include')
with_asan = False
user_rj = False

# Parse arguments
for idx, arg in enumerate(sys.argv[:]):
    if arg.startswith('--rj-include-dir='):
        sys.argv.pop(idx)
        rj_include_dir = arg.split('=', 1)[1]
        user_rj = True
        break
else:
    if not os.path.isdir(os.path.join(ROOT_PATH, 'cpp',
                                      'rapidjson', 'include')):
        raise RuntimeError("RapidJSON sources not found: if you cloned the"
                           " git repository, you should initialize the"
                           " rapidjson submodule as explained in the"
                           " README.rst; in all other cases you may"
                           " want to report the issue.")
for idx, arg in enumerate(sys.argv[:]):
    if arg == '--with-asan':
        sys.argv.pop(idx)
        with_asan = True
        break

# with open('version.txt', encoding='utf-8') as f:
#     VERSION = f.read()

# with open('README.rst', encoding='utf-8') as f:
#     LONG_DESCRIPTION = f.read()

# with open('CHANGES.rst', encoding='utf-8') as f:
#     CHANGES = f.read()


other_setup_options = {
    'cmake_install_dir': 'cpp/YggInterface',
    'cmake_args': [],
    'packages': ['YggInterface'],
    'package_dir': {'': 'cpp'},
}
if with_asan:
    other_setup_options['cmake_args'] += [
        '-DYGG_BUILD_ASAN:BOOL=ON',
        '-DYGG_BUILD_UBSAN:BOOL=ON',
    ]
if user_rj:
    other_setup_options['cmake_args'].append(
        f'-DRAPIDJSON_INCLUDE_DIRS={rj_include_dir}'
    )

setup(
    name='YggInterface',
    # version=VERSION,
    description='Python wrapper around yggdrasil C++',
    # long_description=LONG_DESCRIPTION + '\n\n' + CHANGES,
    # long_description_content_type='text/x-rst',
    license='MIT License',
    keywords='yggdrasil',
    author='Doug Friedel & Meagan Lang',
    author_email='langmm.astro@gmail.com',
    maintainer='Meagan Lang',
    maintainer_email='langmm.astro@gmail.com',
    url='https://github.com/astro-friedel/yggcpp',
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Programming Language :: C++',
        'Programming Language :: Python :: 3 :: Only',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python',
    ],
    setup_requires=['numpy'],
    install_requires=['numpy'],
    **other_setup_options
)
