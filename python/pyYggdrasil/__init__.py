try:
    from pyYggdrasil import _pyYggdrasil  # noqa: F401
except ImportError:
    import os
    import os.path
    import re
    from ctypes import CDLL, PyDLL
    if hasattr(os, 'add_dll_directory'):
        os.add_dll_directory(os.path.dirname(__file__))
    bundled_cpplib = next(
        filter(
            lambda fl: re.match(".*YggInterface_py\\..*", fl),
            sorted(os.listdir(os.path.dirname(__file__))),
        ),
        None,
    )
    bundled_pylib = next(
        filter(
            lambda fl: re.match(".*_pyYggdrasil\\..*", fl),
            sorted(os.listdir(os.path.dirname(__file__))),
        ),
        None,
    )
    if bundled_cpplib:
        CDLL(os.path.join(os.path.dirname(__file__), bundled_cpplib),
             winmode=0)
    elif bundled_pylib:
        PyDLL(os.path.join(os.path.dirname(__file__), bundled_pylib),
              winmode=0)
    else:
        raise FileNotFoundError(
            "YggInterface C++ library is not installed and "
            "no bundled version was detected")
    from pyYggdrasil import _pyYggdrasil  # noqa: F401


from ._pyYggdrasil import (
    CommBase,  # noqa: F401
    CommMeta,  # noqa: F401
    DIRECTION,  # noqa: F401
    COMM_TYPE,  # noqa: F401
    COMM_FLAGS,  # noqa: F401
    is_comm_installed,  # noqa: F401
)
