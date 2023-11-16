try:
    from pyYggdrasil import _pyYggdrasil  # noqa: F401
except ImportError as e:
    import os
    import os.path
    import re
    from ctypes import CDLL
    if hasattr(os, 'add_dll_directory'):
        os.add_dll_directory(os.path.dirname(__file__))

    bundled_lib = next(
        filter(
            lambda fl: re.match(".*YggInterface_py\\..*", fl),
            sorted(os.listdir(os.path.dirname(__file__))),
        ),
        None,
    )
    if bundled_lib:
        CDLL(os.path.join(os.path.dirname(__file__), bundled_lib),
             winmode=0)
    # else:
    #     raise FileNotFoundError(
    #         "YggInterface C++ library is not installed and "
    #         "no bundled version was detected")
    from pyYggdrasil import _pyYggdrasil  # noqa: F401


from ._pyYggdrasil import (
    CommBase,  # noqa: F401
    CommMeta,  # noqa: F401
    DIRECTION,  # noqa: F401
    COMM_TYPE,  # noqa: F401
    COMM_FLAGS,  # noqa: F401
    is_comm_installed,  # noqa: F401
)
