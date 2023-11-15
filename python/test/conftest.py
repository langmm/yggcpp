from subprocess import Popen, PIPE
import numpy as np
import time
import sys
import gc
import pytest


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
