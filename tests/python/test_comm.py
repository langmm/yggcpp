import pytest
import YggInterface
from threading import Thread
import numpy as np
import pickle

_commtypes = [
    YggInterface.COMM_TYPE.IPC_COMM,
    YggInterface.COMM_TYPE.ZMQ_COMM,
    YggInterface.COMM_TYPE.MPI_COMM,
    YggInterface.COMM_TYPE.RMQ_COMM,
]

_commtype_map = {
    'ipc': YggInterface.COMM_TYPE.IPC_COMM,
    'zmq': YggInterface.COMM_TYPE.ZMQ_COMM,
    'mpi': YggInterface.COMM_TYPE.MPI_COMM,
    'rmq': YggInterface.COMM_TYPE.RMQ_COMM,
}
_commtype_map_inv = {v: k for k, v in _commtype_map.items()}
_testdata = [
    ('string', 'Hello world'),
    ('integer', 1),
    ('array', ['a', 1]),
    ('object', {'a': 1, 'b': 'c'}),
    ('ndarray', np.arange(10)),
]


def example_filter(msg):
    print("MSG")
    return (msg == 1)


def example_transform(msg):
    if not (isinstance(msg, int) and msg < 4):
        raise Exception
    return str(msg)


class TestComm_t_Installed:
    r"""Tests for when a commtype is installed."""

    @pytest.fixture(scope="class", params=_commtypes)
    def commtype(self, request):
        return request.param

    @pytest.fixture(scope="class")
    def commtype_str(self, commtype):
        return _commtype_map_inv[commtype]

    @pytest.fixture(params=_testdata)
    def message(self, request):
        return request.param

    @pytest.fixture(scope="class", autouse=True)
    def require_installed(self, commtype):
        if not YggInterface.is_comm_installed(commtype):
            pytest.skip(f"Communicator type {commtype} is not installed")

    @pytest.fixture
    def comm_send(self, commtype, require_installed):
        out = YggInterface.Comm_t(
            "test", commtype=commtype)
        yield out
        out.close()

    @pytest.fixture
    def comm_recv(self, commtype, require_installed):
        out = YggInterface.Comm_t(
            "test", commtype=commtype,
            direction=YggInterface.DIRECTION.RECV)
        yield out
        out.close()

    @pytest.fixture
    def create_comm_partner(self):
        def create_comm_partner_wrapped(comm):
            if comm.direction == YggInterface.DIRECTION.RECV:
                partner_dir = YggInterface.DIRECTION.SEND
            else:
                partner_dir = YggInterface.DIRECTION.RECV
            return YggInterface.Comm_t(
                "test", comm.address, partner_dir, commtype=comm.commtype,
                flags=YggInterface.COMM_FLAG.COMM_FLAG_INTERFACE)
        return create_comm_partner_wrapped

    @pytest.fixture
    def do_recv(self, comm_send, create_comm_partner):

        def do_recv_wrapped(timeout, result_recv_thread):
            comm_recv = create_comm_partner(comm_send)
            comm_recv.timeout_recv = timeout
            result_recv_thread[0] = comm_recv.recv()
            if result_recv_thread[0][0]:
                assert comm_recv.datatype == comm_send.datatype

        return do_recv_wrapped

    @pytest.fixture
    def do_send(self, comm_recv, create_comm_partner):

        def do_send_wrapped(msg, result_send_thread):
            comm_send = create_comm_partner(comm_recv)
            result_send_thread[0] = comm_send.send(msg)

        return do_send_wrapped

    @pytest.fixture
    def do_send_recv(self, comm_recv, do_send, compare_message):

        def do_send_recv_wrapped(msg, msg_type='string'):
            result_send_thread = [False]
            result_recv = (False, None)
            assert comm_recv.n_msg == 0
            thread = Thread(target=do_send, daemon=True,
                            args=(msg, result_send_thread))
            thread.start()
            try:
                comm_recv.timeout_recv = 100000
                assert comm_recv.wait_for_recv(100000) > 0
                assert comm_recv.n_msg > 0
                result_recv = comm_recv.recv()
                assert result_recv
            finally:
                thread.join(timeout=5)
                assert not thread.is_alive()
            assert result_send_thread[0] is not None
            assert result_send_thread[0]
            assert result_recv[0]
            compare_message(result_recv[1], msg)
            assert comm_recv.datatype['type'] == msg_type

        return do_send_recv_wrapped

    def test_pickle(self, commtype, require_installed):
        tmp = YggInterface.Comm_t(
            "test", commtype=commtype, dont_open=True)
        assert not tmp.is_open
        x = pickle.dumps(tmp)
        res = pickle.loads(x)
        tmp.printStatus()
        res.printStatus()
        assert res == tmp
        tmp.open()
        assert tmp.is_open

    def test_is_comm_installed(self, commtype):
        assert YggInterface.is_comm_installed(commtype)

    def test_str_arguments(self, commtype, commtype_str):
        comm_send = YggInterface.Comm_t(
            "test", commtype=commtype_str, direction="send")
        assert comm_send.commtype == commtype
        assert comm_send.direction == YggInterface.DIRECTION.SEND
        comm_recv = YggInterface.Comm_t(
            "test", commtype=commtype_str, direction="recv")
        assert comm_recv.commtype == commtype
        assert comm_recv.direction == YggInterface.DIRECTION.RECV

    def test_str(self, comm_send):
        print(str(comm_send))
        print(repr(comm_send))

    def test_properties(self, comm_send):
        assert comm_send.name == "test"
        assert comm_send.address
        assert comm_send.direction == YggInterface.DIRECTION.SEND
        assert comm_send.commtype

    def test_status(self, comm_send):
        comm_send.printStatus()
        comm_send.printStatus(nindent=1)
        comm_send.printStatus(extra_lines_before=["before"])
        comm_send.printStatus(extra_lines_after=["after"])
        print(comm_send.get_status_message(nindent=1))

    def test_open_close(self, comm_send):
        assert comm_send.is_open
        comm_send.close()
        assert comm_send.is_closed

    def test_metadata(self, comm_send):
        assert not comm_send.metadata
        x = comm_send.metadata
        x['A'] = "A"
        assert 'A' in comm_send.metadata
        assert comm_send.metadata['A'] == 'A'
        assert comm_send.metadata == {'A': 'A'}
        print(repr(x))

    def test_datatype(self, comm_send):
        assert not comm_send.metadata
        with pytest.raises(KeyError):
            comm_send.datatype
        comm_send.datatype = {'type': 'integer'}
        assert comm_send.datatype == {'type': 'integer'}
        with pytest.raises(TypeError):
            comm_send.datatype = {'type': 'string'}

    def test_send_recv_error(self, do_recv):
        result_recv_thread = [None]
        thread = Thread(target=do_recv, daemon=True,
                        args=(100, result_recv_thread))
        thread.start()
        thread.join(5)
        assert not thread.is_alive()
        assert result_recv_thread[0] is not None
        assert not result_recv_thread[0][0]
        assert result_recv_thread[0][1] is None

    def test_send_recv(self, do_send_recv, message):
        do_send_recv(message[1], msg_type=message[0])

    def test_send_recv_async(self, commtype, require_installed):
        comm_recv = YggInterface.Comm_t(
            "test", commtype=commtype,
            direction=YggInterface.DIRECTION.RECV,
            flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        comm_send = YggInterface.Comm_t(
            "test", comm_recv.address, commtype=commtype,
            direction=YggInterface.DIRECTION.SEND,
            flags=(YggInterface.COMM_FLAG.COMM_FLAG_INTERFACE |
                   YggInterface.COMM_FLAG.COMM_FLAG_ASYNC))
        msg = "Test Message"
        assert comm_send.send(msg)
        assert comm_recv.recv() == (True, msg)

    def test_send_recv_filter_recv(self, commtype, require_installed):
        comm_recv = YggInterface.Comm_t(
            "test", commtype=commtype,
            direction=YggInterface.DIRECTION.RECV,
            filter=example_filter,
            flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        comm_send = YggInterface.Comm_t(
            "test", comm_recv.address, commtype=commtype,
            direction=YggInterface.DIRECTION.SEND,
            flags=(YggInterface.COMM_FLAG.COMM_FLAG_INTERFACE |
                   YggInterface.COMM_FLAG.COMM_FLAG_ASYNC))
        assert comm_send.send(0)
        assert comm_send.send(1)
        assert comm_send.send(2)
        assert comm_send.send_eof()
        assert comm_recv.recv() == (True, 0)
        assert comm_recv.recv() == (True, 2)
        assert comm_recv.recv() == (False, None)
        comm_send.close()
        comm_recv.close()

    def test_send_recv_filter_send(self, commtype, require_installed):
        comm_recv = YggInterface.Comm_t(
            "test", commtype=commtype,
            direction=YggInterface.DIRECTION.RECV,
            flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        comm_send = YggInterface.Comm_t(
            "test", comm_recv.address, commtype=commtype,
            direction=YggInterface.DIRECTION.SEND,
            filter=[example_filter],
            flags=(YggInterface.COMM_FLAG.COMM_FLAG_INTERFACE |
                   YggInterface.COMM_FLAG.COMM_FLAG_ASYNC))
        assert comm_send.send(0)
        assert comm_send.send(1)
        assert comm_send.send(2)
        assert comm_send.send_eof()
        assert comm_recv.recv() == (True, 0)
        assert comm_recv.recv() == (True, 2)
        assert comm_recv.recv() == (False, None)
        comm_send.close()
        comm_recv.close()

    def test_send_recv_transform_recv(self, commtype, require_installed):
        comm_recv = YggInterface.Comm_t(
            "test", commtype=commtype,
            direction=YggInterface.DIRECTION.RECV,
            transform=example_transform,
            flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        comm_send = YggInterface.Comm_t(
            "test", comm_recv.address, commtype=commtype,
            direction=YggInterface.DIRECTION.SEND,
            flags=(YggInterface.COMM_FLAG.COMM_FLAG_INTERFACE |
                   YggInterface.COMM_FLAG.COMM_FLAG_ASYNC))
        assert comm_send.send(0)
        assert comm_send.send(1)
        assert comm_send.send(2)
        assert comm_send.send_eof()
        assert comm_recv.recv() == (True, "0")
        assert comm_recv.recv() == (True, "1")
        assert comm_recv.recv() == (True, "2")
        assert comm_recv.recv() == (False, None)
        comm_send.close()
        comm_recv.close()

    def test_send_recv_transform_send(self, commtype, require_installed):
        comm_recv = YggInterface.Comm_t(
            "test", commtype=commtype,
            direction=YggInterface.DIRECTION.RECV,
            flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        comm_send = YggInterface.Comm_t(
            "test", comm_recv.address, commtype=commtype,
            direction=YggInterface.DIRECTION.SEND,
            transform=[example_transform],
            flags=(YggInterface.COMM_FLAG.COMM_FLAG_INTERFACE |
                   YggInterface.COMM_FLAG.COMM_FLAG_ASYNC))
        assert comm_send.send(0)
        assert comm_send.send(1)
        assert comm_send.send(2)
        assert comm_send.send_eof()
        assert comm_recv.recv() == (True, "0")
        assert comm_recv.recv() == (True, "1")
        assert comm_recv.recv() == (True, "2")
        assert comm_recv.recv() == (False, None)
        comm_send.close()
        comm_recv.close()

    def test_send_dict_recv(self, commtype, require_installed):
        comm_recv = YggInterface.Comm_t(
            "test", commtype=commtype,
            direction=YggInterface.DIRECTION.RECV,
            flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        comm_send = YggInterface.Comm_t(
            "test", comm_recv.address, commtype=commtype,
            direction=YggInterface.DIRECTION.SEND,
            flags=(YggInterface.COMM_FLAG.COMM_FLAG_INTERFACE |
                   YggInterface.COMM_FLAG.COMM_FLAG_ASYNC))
        key_order = ["a", "b", "c"]
        msg_send = ["a", 1, None]
        msg_recv = {k: v for k, v in zip(key_order, msg_send)}
        assert comm_send.send_dict(msg_send, key_order=key_order)
        assert comm_recv.recv() == (True, msg_recv)

    def test_send_recv_dict(self, commtype, require_installed):
        comm_recv = YggInterface.Comm_t(
            "test", commtype=commtype,
            direction=YggInterface.DIRECTION.RECV,
            flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        comm_send = YggInterface.Comm_t(
            "test", comm_recv.address, commtype=commtype,
            direction=YggInterface.DIRECTION.SEND,
            flags=(YggInterface.COMM_FLAG.COMM_FLAG_INTERFACE |
                   YggInterface.COMM_FLAG.COMM_FLAG_ASYNC))
        key_order = ["a", "b", "c"]
        msg_send = ["a", 1, None]
        msg_recv = {k: v for k, v in zip(key_order, msg_send)}
        assert comm_send.send(msg_send)
        assert comm_recv.recv_dict(key_order=key_order) == (True, msg_recv)

    def test_send_recv_long(self, comm_recv, do_send_recv):
        if comm_recv.maxMsgSize == 0:
            pytest.skip("Communicator does not have a maxMsgSize")
        do_send_recv("Hello world" + comm_recv.maxMsgSize * "0")

    def test_call(self, comm_send):
        assert comm_send.call("Hello") == (False, None)


class TestComm_t_NotInstalled:
    r"""Tests for when a commtype is not installed."""

    @pytest.fixture(scope="class", params=_commtypes)
    def commtype(self, request):
        return request.param

    @pytest.fixture(autouse=True)
    def require_not_installed(self, commtype):
        if YggInterface.is_comm_installed(commtype):
            pytest.skip(f"Communicator type {commtype} is installed")

    def test_is_comm_installed(self, commtype):
        assert not YggInterface.is_comm_installed(commtype)

    def test_error_on_create(self, commtype):
        with pytest.raises(TypeError):
            YggInterface.Comm_t("test", commtype=commtype)


class TestRPC:
    r"""Tests for RPC client/server connection pattern."""

    @pytest.fixture(params=_testdata)
    def request_message(self, request):
        return request.param

    @pytest.fixture(params=(_testdata[1:] + _testdata))
    def response_message(self, request):
        return request.param

    @pytest.fixture
    def server(self):
        out = YggInterface.Comm_t(
            "test_server", commtype=YggInterface.COMM_TYPE.SERVER_COMM)
        yield out
        out.close()

    @pytest.fixture
    def create_comm_partner(self):
        def create_comm_partner_wrapped(comm, flags=0):
            if comm.commtype == YggInterface.COMM_TYPE.SERVER_COMM:
                partner_name = "client"
                partner_commtype = YggInterface.COMM_TYPE.CLIENT_COMM
            else:
                partner_name = "server"
                partner_commtype = YggInterface.COMM_TYPE.SERVER_COMM
            flags |= YggInterface.COMM_FLAG.COMM_FLAG_INTERFACE
            return YggInterface.Comm_t(
                "test_" + partner_name, comm.address,
                commtype=partner_commtype, flags=flags)
        return create_comm_partner_wrapped

    @pytest.fixture
    def do_call(self, create_comm_partner):

        def do_call_wrapped(server, msg, result_call_thread, **kwargs):
            client = create_comm_partner(server, **kwargs)
            client.timeout_recv = 1000000
            result_call_thread[0] = client.call(msg)
            client.close()

        return do_call_wrapped

    @pytest.fixture
    def do_rpc(self, do_call, compare_message):

        def do_rpc_wrapped(server, req, res, req_type='string',
                           res_type='string', **kwargs):
            result_call_thread = [None]
            result_recv = (False, None)
            result_send = False
            server.timeout_recv = 1000000
            thread = Thread(target=do_call, daemon=True,
                            args=(server, req, result_call_thread),
                            kwargs=kwargs)
            thread.start()
            try:
                # Request
                result_recv = server.recv()
                assert result_recv
                assert result_recv[0]
                compare_message(result_recv[1], req)
                # Response
                result_send = server.send(res)
                assert result_send
            finally:
                thread.join(timeout=5)
                assert not thread.is_alive()
            assert result_call_thread[0] is not None
            assert result_call_thread[0][0]
            compare_message(result_call_thread[0][1], res)
            assert server.datatype['type'] == req_type

        return do_rpc_wrapped

    def test_call(self, server, do_rpc, request_message,
                  response_message):
        do_rpc(server, request_message[1], response_message[1],
               req_type=request_message[0], res_type=response_message[0])

    def test_call_long(self, server, do_rpc):
        if server.maxMsgSize == 0:
            pytest.skip("Communicator does not have a maxMsgSize")
        do_rpc(server,
               "REQUEST" + server.maxMsgSize * "0",
               "RESPONSE" + server.maxMsgSize * "0")

    def test_send_recv_async(self):
        server = YggInterface.Comm_t(
            "test_server",
            commtype=YggInterface.COMM_TYPE.SERVER_COMM,
            flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        client = YggInterface.Comm_t(
            "test_client", server.address,
            commtype=YggInterface.COMM_TYPE.CLIENT_COMM,
            flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        req = "REQUEST"
        res = "RESPONSE"
        server.timeout_recv = 1000000
        client.timeout_recv = 1000000
        assert client.send(req)
        assert server.recv() == (True, req)
        assert server.send(res)
        assert client.recv() == (True, res)
        client.close()
        server.close()

    def test_call_async(self, do_rpc):
        server = YggInterface.Comm_t(
            "test_server",
            commtype=YggInterface.COMM_TYPE.SERVER_COMM,
            flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        do_rpc(server, "REQUEST", "RESPONSE",
               flags=YggInterface.COMM_FLAG.COMM_FLAG_ASYNC)
        server.close()
