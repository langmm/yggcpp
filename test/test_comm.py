import pytest
import pyYggdrasil

class TestComm_t:

    @pytest.fixture
    def comm(self):
        return pyYggdrasil.CommBase("test")

    def test_str(self, comm):
        print(str(comm))
        print(repr(comm))
        
