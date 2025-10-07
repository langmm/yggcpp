#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/BufferComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"


using namespace YggInterface;
using namespace YggInterface::communicator;
using namespace YggInterface::mock;

class BufferComm_tester : public BufferComm {
public:
  TESTER_METHODS(BufferComm)
};

COMM_SERI_TEST(BufferComm)
