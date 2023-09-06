#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/FileComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"


using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;

class FileComm_tester : public FileComm {
public:
  TESTER_METHODS(FileComm)
};

COMM_SERI_TEST(FileComm)
