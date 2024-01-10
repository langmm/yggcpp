#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "utils/tools.hpp"
#include "communicators/comms.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"

using namespace YggInterface;
using namespace YggInterface::communicator;
using namespace YggInterface::mock;

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)

#define DO_MPI_MOCK_OUT(method)				\
  int out = MPISTATUS;					\
  std::cerr << #method << ": COUNT = " << COUNT <<	\
    ", MPISTATUS = " << MPISTATUS << std::endl;		\
  if (COUNT_ALT >= 0 && COUNT >= COUNT_ALT)		\
    MPISTATUS = MPISTATUS_ALT;				\
  COUNT++;						\
  CheckReturn(out, #method);				\
  return out

class mpi_registry_mock : public mpi_registry_t {
public:
    mpi_registry_mock(MPI_Comm comm) : mpi_registry_t(comm) {
        procs = {51000, 50000};
        MPISTATUS = 0;
        MPICANCEL = false;
        MPIPROC = 50000;
	COUNT = 0;
	COUNT_ALT = -1;
	MPISTATUS_ALT = 0;
	msg = "\"This is a message\"";
    }
    int Probe(int, MPI_Status *status) const override {
        MPI_Status_set_cancelled(status, MPICANCEL);
	status->MPI_ERROR = MPISTATUS;
	status->MPI_SOURCE = 0;
        if (MPIPROC > 0)
	    status->MPI_SOURCE = MPIPROC;
	DO_MPI_MOCK_OUT(Probe);
    }

    int Send(const void* buf, int, MPI_Datatype dt, int) const override {
        if (dt != MPI_Datatype(MPI_INT)) {
	  msg.assign((char*)buf);
	}
        DO_MPI_MOCK_OUT(Send);
    }

    int Recv(void* buf, int, MPI_Datatype dt, int, MPI_Status* status) const override {
        MPI_Status_set_cancelled(status, MPICANCEL);
        status->MPI_ERROR = MPISTATUS;
        char* cmsg = const_cast<char*>(msg.c_str());
        int sz = static_cast<int>(msg.size());
        if(dt == MPI_Datatype(MPI_INT)) {
            memcpy(buf, &sz, sizeof(int));
        } else {
	    memcpy(buf, cmsg, sizeof(char) * static_cast<size_t>(sz));
        }
	DO_MPI_MOCK_OUT(Recv);
    }
    static int MPISTATUS;
    static bool MPICANCEL;
    static int MPIPROC;
    static int COUNT;
    static int COUNT_ALT;
    static int MPISTATUS_ALT;
    static std::string msg;
};

int mpi_registry_mock::MPISTATUS = 0;
bool mpi_registry_mock::MPICANCEL = false;
int mpi_registry_mock::MPIPROC = 0;
int mpi_registry_mock::COUNT = 0;
int mpi_registry_mock::COUNT_ALT = 0;
int mpi_registry_mock::MPISTATUS_ALT = 0;
std::string mpi_registry_mock::msg = "\"This is a message\"";

class MPIComm_tester : public MPIComm {
public:
  MPIComm_tester(const std::string name,
		 utils::Address& address,
		 const DIRECTION direction = NONE) :
    MPIComm(name, address, direction), tmp(0) { init(); }
  MPIComm_tester(DIRECTION dir) :
    MPIComm("", dir), tmp(0) { init(); }
  MPIComm_tester(const std::string name, DIRECTION dir) :
    MPIComm(name, dir), tmp(0) { init(); }
private:
  MPIComm_tester(const MPIComm_tester&) = delete;
  MPIComm_tester& operator=(const MPIComm_tester&) = delete;
public:
  ~MPIComm_tester() {
    restore();
  }
  void init() {
    tmp = getHandle();
    setHandle(new mpi_registry_mock(MPI_COMM_WORLD));
  }
  void restore() {
    if (tmp) {
      mpi_registry_t* h = getHandle();
      setHandle(tmp);
      delete h;
      tmp = nullptr;
    }
  }
  mpi_registry_t* tmp;
};

#define INIT_MPI_TEST				\
  int stat;					\
  MPI_Initialized(&stat);			\
  if (!stat) MPI_Init(nullptr, nullptr)

COMM_SERI_TEST_BASE(MPIComm, INIT_MPI_TEST)

TEST(MPIComm, constructor) {
    INIT_MPI_TEST;
    std::string name = "TestMPIComm";
    // EXPECT_THROW(MPIComm mpic(name, nullptr, SEND), std::runtime_error);
    utils::Address adr("50000,51000");
    MPIComm_tester mpic(name, adr, SEND);
    EXPECT_EQ(mpic.getAddresses().size(), 2);
    name = "";
    utils::Address adr2("50000,51000");
    MPIComm mpic2(name, adr2, SEND);
    EXPECT_NE(mpic2.getName().find("tempinitMPI"), std::string::npos);
    utils::Address adr3("[50000], 51000");
    MPIComm_tester mpic3(name, adr3, RECV);
    std::vector<utils::Address> adrlist = mpic3.getAddresses();
    EXPECT_EQ(adrlist.size(), 2);
    EXPECT_EQ(adrlist[0].address(), "[50000]");
    EXPECT_EQ(adrlist[1].address(), "51000");
}

TEST(MPIComm, sourceID) {
    INIT_MPI_TEST;
    std::string name = "";
    utils::Address adr("50000");
    MPIComm mpic(name, adr, SEND);
    mpi_registry_mock::MPIPROC = 0;
    EXPECT_EQ(mpic.mpi_comm_source_id(), 0);

    utils::Address adr2("51000,50000");
    MPIComm_tester mpic2(name, adr2, RECV);
    mpi_registry_mock::MPIPROC = 0;
    EXPECT_EQ(mpic2.mpi_comm_source_id(), 0);

    mpi_registry_mock::MPISTATUS = 1;
    EXPECT_EQ(mpic2.mpi_comm_source_id(), -1);

    mpi_registry_mock::MPISTATUS = 0;
    mpi_registry_mock::MPICANCEL = true;

    EXPECT_EQ(mpic2.mpi_comm_source_id(), -1);

    mpi_registry_mock::MPICANCEL = false;
    mpi_registry_mock::MPIPROC = 50000;
    EXPECT_EQ(mpic2.mpi_comm_source_id(), 50000);

    mpi_registry_mock* h = (mpi_registry_mock*)(mpic2.getHandle());
    mpic2.setHandle(nullptr);
    EXPECT_EQ(mpic2.mpi_comm_source_id(), -1);
    mpic2.setHandle(h);

    //MPI_Finalize();
}

TEST(MPIComm, commnmsg) {
    INIT_MPI_TEST;
    utils::Address adr("51000,50000");
    MPIComm_tester mpic("", adr, RECV);
    mpi_registry_mock::MPIPROC = 0;
    EXPECT_EQ(mpic.comm_nmsg(), 0);

    mpi_registry_mock::MPIPROC = 50000;
    EXPECT_EQ(mpic.comm_nmsg(), 1);
    mpi_registry_mock::MPISTATUS = 1;
    EXPECT_EQ(mpic.comm_nmsg(), -1);
    //MPI_Finalize();
}

TEST(MPIComm, send) {
    INIT_MPI_TEST;
    utils::Address adr("51000,50000");
    MPIComm_tester mpic("", adr, SEND);

    EXPECT_GT(mpic.send("Hello", 6), 0);
    mpi_registry_mock::MPISTATUS = 2;
    EXPECT_EQ(mpic.send("Hello", 6), -1);
    mpi_registry_mock::MPISTATUS = 0;
    mpi_registry_mock::MPISTATUS_ALT = 2;
    mpi_registry_mock::COUNT = 0;
    mpi_registry_mock::COUNT_ALT = 0;
    EXPECT_EQ(mpic.send("Hello", 6), -1);
    mpi_registry_mock* h = (mpi_registry_mock*)(mpic.getHandle());
    mpic.setHandle(nullptr);
    EXPECT_EQ(mpic.send("hello", 6), -1);
    mpic.setHandle(h);
    mpic.restore();
#ifdef ELF_AVAILABLE
    ELF_BEGIN;
    ELF_REPLACE_SEND_MPI;
    // Error in MPI_Send
    EXPECT_EQ(mpic.send("Hello", 6), -1);
    ELF_RESTORE_SEND_MPI;
    ELF_END;
#endif // ELF_AVAILABLE
}

TEST(MPIComm, recv) {
    INIT_MPI_TEST;
    utils::Address adr("51000,50000");
    MPIComm_tester mpic("", adr, RECV);
    char* data = (char*)malloc(sizeof(char) * 1);
    size_t len = 1;
    mpic.set_timeout_recv(1000);
    mpi_registry_mock::MPIPROC = 50000;
    EXPECT_EQ(mpic.recv(data, len, false), -((long)mpi_registry_mock::msg.size() - 2));
    EXPECT_EQ(mpic.recv(data, len, true),
	      mpi_registry_mock::msg.size() - 2);
    mpi_registry_mock::MPISTATUS = 2;
    EXPECT_EQ(mpic.recv(data, len, true), -1);
    mpi_registry_mock::MPISTATUS = 0;
    EXPECT_EQ(mpic.recv(data, len, true),
	      mpi_registry_mock::msg.size() - 2);
    mpi_registry_mock::MPISTATUS = 0;
    mpi_registry_mock::MPISTATUS_ALT = MPI_ERR_COMM;
    mpi_registry_mock::COUNT = 0;
    mpi_registry_mock::COUNT_ALT = 1;
    EXPECT_EQ(mpic.recv(data, len, true), -1);
    mpi_registry_mock::MPISTATUS = 0;
    mpi_registry_mock::MPISTATUS_ALT = MPI_ERR_TAG;
    mpi_registry_mock::COUNT = 0;
    mpi_registry_mock::COUNT_ALT = 2;
    EXPECT_EQ(mpic.recv(data, len, true), -1);
    mpi_registry_mock::MPISTATUS = 0;
    mpi_registry_mock::MPISTATUS_ALT = MPI_ERR_RANK;
    mpi_registry_mock::COUNT = 0;
    mpi_registry_mock::COUNT_ALT = 3;
    EXPECT_EQ(mpic.recv(data, len, true), -1);
    mpi_registry_mock::MPISTATUS = MPI_ERR_TYPE;
    mpi_registry_mock::COUNT = 0;
    mpi_registry_mock::COUNT_ALT = -1;
    EXPECT_EQ(mpic.recv(data, len, true), -1);
    free(data);
    data = NULL;
    len = 0;
    mpic.restore();
#ifdef ELF_AVAILABLE
    ELF_BEGIN;
    ELF_RECV_T(MPI, -1);
    // Error in MPI_Probe
    EXPECT_EQ(mpic.recv(data, len, true), -1);
    // Error in MPI_Recv
    RETVAL = 2;
    RETVAL_INC_POLL = -1;
    EXPECT_EQ(mpic.recv(data, len, true), -1);
    // Error in realloc
    RETVAL = 0;
    RETVAL_INC_POLL = 0;
    EXPECT_EQ(mpic.recv(data, len, false), -1);
    ELF_RECV_REVERT_T(MPI);
    ELF_END;
#endif // ELF_AVAILABLE
}

// TODO: Add tests for workers, and large message

#else // MPIINSTALLED

TEST(MPIComm, errors) {
  //EXPECT_THROW(MPIComm mpi, std::exception);
  //std::string name = "";
  //EXPECT_THROW(MPIComm mpi2(name, nullptr, SEND), std::exception);
}

#endif // MPIINSTALLED
