import pytest
import pyYggdrasil


class TestComm_t:

    @pytest.fixture
    def comm_send(self):
        return pyYggdrasil.CommBase("test")

    @pytest.fixture
    def comm_recv(self, comm_send):
        return pyYggdrasil.CommBase("test", comm_send.address,
                                    pyYggdrasil.DIRECTION.RECV)

    def test_str(self, comm_send):
        print(str(comm_send))
        print(repr(comm_send))

    def test_properties(self, comm_send, comm_recv):
        assert comm_send.name == "test"
        assert comm_send.address
        assert comm_send.direction == pyYggdrasil.DIRECTION.SEND
        assert comm_recv.direction == pyYggdrasil.DIRECTION.RECV
        assert comm_send.commtype

    def test_metadata(self, comm_send):
        x = comm_send.metadata
        print(x)

    # TODO
    def test_send_recv(self, comm_send, comm_recv):
        pass
