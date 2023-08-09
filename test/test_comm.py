import pytest
import pyYggdrasil
from threading import Thread


# TODO: Parameterize for IPC, ZMQ, MPI

class TestComm_t:

    @pytest.fixture(scope="class")
    def commtype(self):
        return pyYggdrasil.COMM_TYPE.IPC_COMM

    @pytest.fixture(autouse=True)
    def require_installed(commtype):
        # TODO: Add Python method for checking if a comm is installed
        pass

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
                flags=0x00002000)
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

    def test_str(self, comm_send):
        print(str(comm_send))
        print(repr(comm_send))

    def test_properties(self, comm_send):
        assert comm_send.name == "test"
        assert comm_send.address
        assert comm_send.direction == pyYggdrasil.DIRECTION.SEND
        assert comm_send.commtype

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
        thread = Thread(target=do_recv,
                        args=(100, result_recv_thread))
        thread.start()
        thread.join(1)
        assert result_recv_thread[0] is not None
        assert not result_recv_thread[0][0]
        assert result_recv_thread[0][1] is None

    # def test_send_recv(self, comm_send, do_recv):
    #     msg = "Hello world"
    #     result_send = False
    #     result_recv_thread = [None]
    #     thread = Thread(target=do_recv,
    #                     args=(10000, result_recv_thread))
    #     thread.start()
    #     try:
    #         result_send = comm_send.send(msg)
    #         assert result_send
    #     finally:
    #         thread.join(timeout=1)
    #     assert comm_send.datatype['type'] == 'string'
    #     assert result_send
    #     assert result_recv_thread[0] is not None
    #     assert result_recv_thread[0][0]
    #     assert result_recv_thread[0][1] == msg

    def test_send_recv(self, comm_recv, do_send):
        msg = "Hello world"
        result_send_thread = [False]
        result_recv = (False, None)
        thread = Thread(target=do_send,
                        args=(msg, result_send_thread))
        thread.start()
        try:
            comm_recv.timeout_recv = 10000
            result_recv = comm_recv.recv()
            assert result_recv
        finally:
            thread.join(timeout=1)
        assert comm_recv.datatype['type'] == 'string'
        assert result_send_thread[0] is not None
        assert result_send_thread[0]
        assert result_recv[0]
        assert result_recv[1] == msg
