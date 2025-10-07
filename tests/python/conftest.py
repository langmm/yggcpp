from subprocess import Popen, PIPE
import numpy as np
import time
import sys
import gc
import os
import pytest


def pytest_addoption(parser):
    parser.addoption("--dynamic-testlib-dir", action="store",
                     default=os.path.dirname(os.path.dirname(__file__)))
    parser.addoption("--external-testlib", action="append",
                     default=[])


def pytest_generate_tests(metafunc):
    if "external_testlib" in metafunc.fixturenames:
        metafunc.parametrize(
            "external_testlib",
            metafunc.config.getoption("external_testlib"),
            scope="class")


@pytest.fixture(scope="session")
def dynamic_testlib_dir(pytestconfig):
    return pytestconfig.getoption("dynamic_testlib_dir")


def ipc_queue_count():
    if sys.platform in ['win32', 'cygwin']:
        return 0
    skip_lines = [
        # Linux
        '------ Message Queues --------',
        'key        msqid      owner      perms      used-bytes   '
        'messages    ',
        # MacOS
        'IPC status from',
        'Message Queues:',
        'T     ID     KEY        MODE       OWNER    GROUP']
    p = Popen('ipcs -q', stdin=PIPE, stdout=PIPE,
              stderr=PIPE, shell=True)
    output, err = p.communicate()
    exit_code = p.returncode
    if exit_code != 0:  # pragma: debug
        if not err.isspace():
            print(err.decode('utf-8'))
            raise Exception("Error on spawned process. See output.")
    lines = output.decode('utf-8').split('\n')
    count = 0
    for x in lines:
        skip = False
        if len(x) == 0:
            skip = True
        else:
            for y in skip_lines:
                if y in x:
                    skip = True
                    break
        if not skip:
            count += 1
    return count


@pytest.fixture(scope="function", autouse=True)
def check_ipc_cleanup():
    count = ipc_queue_count()
    yield
    gc.collect()
    start_time = time.time()
    while ipc_queue_count() != count and (time.time() - start_time) < 1:
        gc.collect()
        time.sleep(0.01)
    assert ipc_queue_count() == count


@pytest.fixture
def compare_message():

    def compare_message_wrapped(actual, expected):
        if isinstance(expected, np.ndarray):
            assert isinstance(actual, np.ndarray)
            assert (actual == expected).all()
        else:
            assert actual == expected

    return compare_message_wrapped


@pytest.fixture(scope="session")
def dynamic_library_ext():
    if sys.platform in ['win32', 'cygwin']:
        return '.dll'
    elif sys.platform.startswith('linux'):
        return '.so'
    elif sys.platform == 'darwin':
        return '.dylib'
    raise NotImplementedError(f"Cannot determine dynamic library file "
                              f"extension for platform {sys.platform}")
