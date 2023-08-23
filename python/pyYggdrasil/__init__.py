try:
    from pyYggdrasil import _pyYggdrasil  # noqa: F401
except ImportError:
    import os
    print(os.path.dirname(__file__))
    print(os.listdir(os.path.dirname(__file__)))
    os.add_dll_directory(os.path.dirname(__file__))
    # import os.path
    # import re
    # from ctypes import cdll

    # bundled_lib = next(
    #     filter(
    #         lambda fl: re.match(".*YggInterface_py\\..*", fl),
    #         sorted(os.listdir(os.path.dirname(__file__))),
    #     ),
    #     None,
    # )
    # if not bundled_lib:
    #     raise FileNotFoundError(
    #         "YggInterface C++ library is not installed and "
    #         "no bundled version was detected")
    # cdll.LoadLibrary(
    #     os.path.join(os.path.dirname(__file__), bundled_lib))
    from pyYggdrasil import _pyYggdrasil  # noqa: F401


from ._pyYggdrasil import (
    CommBase,  # noqa: F401
    CommMeta,  # noqa: F401
    DIRECTION,  # noqa: F401
    COMM_TYPE,  # noqa: F401
    COMM_FLAGS,  # noqa: F401
    is_comm_installed,  # noqa: F401
)
