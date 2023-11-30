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

#ifdef _MSC_VER
COMM_SERI_TEST_FAILED_ASYNC(FileComm)
#else // _MSC_VER
COMM_SERI_TEST(FileComm)
#endif // _MSC_VER
