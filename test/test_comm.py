import pytest
import pyYggdrasil
from threading import Thread

_commtypes = [
    pyYggdrasil.COMM_TYPE.IPC_COMM,
    pyYggdrasil.COMM_TYPE.ZMQ_COMM,
    pyYggdrasil.COMM_TYPE.MPI_COMM,
]


class TestComm_t_Installed:
    r"""Tests for when a commtype is installed."""

    @pytest.fixture(scope="class", params=_commtypes)
    def commtype(self, request):
        return request.param

    @pytest.fixture(scope="class", autouse=True)
    def require_installed(self, commtype):
        if not pyYggdrasil.is_comm_installed(commtype):
            pytest.skip(f"Communicator type {commtype} is not installed")

    @pytest.fixture
    def comm_send(self, commtype, require_installed):
        return pyYggdrasil.CommBase(
            "test", commtype=commtype)

    @pytest.fixture
    def comm_recv(self, commtype, require_installed):
        return pyYggdrasil.CommBase(
            "test", commtype=commtype,
            direction=pyYggdrasil.DIRECTION.RECV)

    @pytest.fixture
    def create_comm_partner(self):
        def create_comm_partner_wrapped(comm):
            if comm.direction == pyYggdrasil.DIRECTION.RECV:
                partner_dir = pyYggdrasil.DIRECTION.SEND
            else:
                partner_dir = pyYggdrasil.DIRECTION.RECV
            return pyYggdrasil.CommBase(
                "test", comm.address, partner_dir, commtype=comm.commtype,
                flags=pyYggdrasil.COMM_FLAGS.COMM_FLAG_INTERFACE)
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
    def do_send_recv(self, comm_recv, do_send):

        def do_send_recv_wrapped(msg):
            result_send_thread = [False]
            result_recv = (False, None)
            thread = Thread(target=do_send, daemon=True,
                            args=(msg, result_send_thread))
            thread.start()
            try:
                comm_recv.timeout_recv = 100000
                result_recv = comm_recv.recv()
                assert result_recv
            finally:
                thread.join(timeout=1)
            assert comm_recv.datatype['type'] == 'string'
            assert result_send_thread[0] is not None
            assert result_send_thread[0]
            assert result_recv[0]
            assert result_recv[1] == msg

        return do_send_recv_wrapped

    def test_is_comm_installed(self, commtype):
        assert pyYggdrasil.is_comm_installed(commtype)

    def test_str(self, comm_send):
        print(str(comm_send))
        print(repr(comm_send))

    def test_properties(self, comm_send):
        assert comm_send.name == "test"
        assert comm_send.address
        assert comm_send.direction == pyYggdrasil.DIRECTION.SEND
        assert comm_send.commtype

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
        thread.join(1)
        assert result_recv_thread[0] is not None
        assert not result_recv_thread[0][0]
        assert result_recv_thread[0][1] is None

    def test_send_recv(self, do_send_recv):
        do_send_recv("Hello world")

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
        if pyYggdrasil.is_comm_installed(commtype):
            pytest.skip(f"Communicator type {commtype} is installed")

    def test_is_comm_installed(self, commtype):
        assert not pyYggdrasil.is_comm_installed(commtype)

    def test_error_on_create(self, commtype):
        with pytest.raises(TypeError):
            pyYggdrasil.CommBase("test", commtype=commtype)


class TestRPC:
    r"""Tests for RPC client/server connection pattern."""

    @pytest.fixture
    def server(self):
        return pyYggdrasil.CommBase(
            "test_server", commtype=pyYggdrasil.COMM_TYPE.SERVER_COMM)

    @pytest.fixture
    def create_comm_partner(self):
        def create_comm_partner_wrapped(comm):
            if comm.commtype == pyYggdrasil.COMM_TYPE.SERVER_COMM:
                partner_name = "client"
                partner_commtype = pyYggdrasil.COMM_TYPE.CLIENT_COMM
            else:
                partner_name = "server"
                partner_commtype = pyYggdrasil.COMM_TYPE.SERVER_COMM
            return pyYggdrasil.CommBase(
                "test_" + partner_name, comm.address,
                commtype=partner_commtype, flags=0x00002000)
        return create_comm_partner_wrapped

    @pytest.fixture
    def do_call(self, server, create_comm_partner):

        def do_call_wrapped(msg, result_call_thread):
            client = create_comm_partner(server)
            client.timeout_recv = 100000
            result_call_thread[0] = client.call(msg)

        return do_call_wrapped

    @pytest.fixture
    def do_rpc(self, server, do_call):

        def do_rpc_wrapped(req, res):
            result_call_thread = [None]
            result_recv = (False, None)
            result_send = False
            thread = Thread(target=do_call, daemon=True,
                            args=(req, result_call_thread))
            thread.start()
            try:
                server.timeout_recv = 100000
                # Request
                result_recv = server.recv()
                assert result_recv
                assert result_recv[0]
                assert result_recv[1] == req
                # Response
                result_send = server.send(res)
                assert result_send
            finally:
                thread.join(timeout=1)
            assert server.datatype['type'] == 'string'
            assert result_call_thread[0] is not None
            assert result_call_thread[0][0]
            assert result_call_thread[0][1] == res

        return do_rpc_wrapped

    def test_call(self, do_rpc):
        do_rpc("REQUEST", "RESPONSE")

    def test_call_long(self, server, do_rpc):
        if server.maxMsgSize == 0:
            pytest.skip("Communicator does not have a maxMsgSize")
        do_rpc("REQUEST" + server.maxMsgSize * "0",
               "RESPONSE" + server.maxMsgSize * "0")
