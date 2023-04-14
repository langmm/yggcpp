#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "utils/tools.hpp"
#include "communicators/MPIComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include <dlfcn.h>

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)

using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;

const std::string msg = "This is a message";

namespace communication {
namespace testing {

class MPIComm_tester : public MPIComm {
public:
    MPIComm_tester(const std::string &name, utils::Address *address, const DIRECTION direction) :
            MPIComm(name, address, direction) {}

    std::vector<utils::Address*> getAddresses() {return addresses;}
    mpi_registry_t* getHandle() {return handle;}
    void setHandle(mpi_registry_t* h) { handle = h;}
    int send(const char *data, const size_t &len) {return MPIComm::send(data, len);}
    long recv(char* data, const size_t &len, bool allow_realloc) {
        return MPIComm::recv(data, len, allow_realloc);
    }
};

class mpi_registry_mock : public communication::communicator::mpi_registry_t {
public:
    mpi_registry_mock(MPI_Comm comm) : communication::communicator::mpi_registry_t(comm) {
        procs = {51000, 50000};
        MPISTATUS = 0;
        MPICANCEL = false;
        MPIPROC = 0;
    }
    int Probe(int source, MPI_Status *status) const override {
        status->_cancelled = MPICANCEL;
	status->MPI_ERROR = MPISTATUS;
        if (MPIPROC > 0)
	    status->MPI_SOURCE = MPIPROC;
	return MPISTATUS;
    }

    int Send(const void* buf, int count, MPI_Datatype dt, int dest) const override {
        return MPISTATUS;
    }

    int Recv(void* buf, int count, MPI_Datatype dt, int source, MPI_Status* status) const override {
        status->_cancelled = MPICANCEL;
        status->MPI_ERROR = MPISTATUS;
        char* cmsg = const_cast<char*>(msg.c_str());
        int sz = msg.size();
        if(dt == MPI_Datatype(MPI_INT)) {
            memcpy(buf, &sz, sizeof(int));
        } else {
            memcpy(buf, cmsg, sizeof(char) * sz);
        }
	return MPISTATUS;
    }
    static int MPISTATUS;
    static bool MPICANCEL;
    static int MPIPROC;
};

int mpi_registry_mock::MPISTATUS = 0;
bool mpi_registry_mock::MPICANCEL = false;
int mpi_registry_mock::MPIPROC = 0;
}
}
TEST(MPICOMM, constructor) {
    std::string name = "TestMPIComm";
    EXPECT_THROW(MPIComm mpic(name, nullptr, SEND), std::runtime_error);
    utils::Address adr("50000,51000");
    communication::testing::MPIComm_tester mpic(name, &adr, SEND);
    EXPECT_EQ(mpic.getAddresses().size(), 2);
    name = "";
    MPIComm mpic2(name, &adr, SEND);
    EXPECT_NE(mpic2.getName().find("tempinitMPI"), std::string::npos);
    utils::Address adr2("[50000], 51000");
    communication::testing::MPIComm_tester mpic3(name, &adr2, RECV);
    std::vector<utils::Address*> adrlist = mpic3.getAddresses();
    EXPECT_EQ(adrlist.size(), 2);
    EXPECT_EQ(adrlist[0]->address(), "[50000]");
    EXPECT_EQ(adrlist[1]->address(), "51000");
}

TEST(MPICOMM, sourceID) {
    int stat;
    MPI_Initialized(&stat);
    if (!stat)
        MPI_Init(nullptr, nullptr);
    std::string name = "";
    MPIComm mpic(name, new utils::Address("50000"), SEND);
    EXPECT_EQ(mpic.mpi_comm_source_id(), 0);

    communication::testing::MPIComm_tester mpic2(name, new utils::Address("51000,50000"), RECV);
    mpi_registry_t *tempmpi = mpic2.getHandle();
    communication::testing::mpi_registry_mock *mock_handle = new communication::testing::mpi_registry_mock(MPI_COMM_WORLD);
    mpic2.setHandle(mock_handle);
    EXPECT_EQ(mpic2.mpi_comm_source_id(), 0);

    communication::testing::mpi_registry_mock::MPISTATUS = 1;
    EXPECT_EQ(mpic2.mpi_comm_source_id(), -1);

    communication::testing::mpi_registry_mock::MPISTATUS = 0;
    communication::testing::mpi_registry_mock::MPICANCEL = true;

    EXPECT_EQ(mpic2.mpi_comm_source_id(), -1);

    communication::testing::mpi_registry_mock::MPICANCEL = false;
    communication::testing::mpi_registry_mock::MPIPROC = 50000;
    EXPECT_EQ(mpic2.mpi_comm_source_id(), 50000);

    mpic2.setHandle(nullptr);
    EXPECT_EQ(mpic2.mpi_comm_source_id(), -1);

    mpic2.setHandle(tempmpi);
    delete mock_handle;
    //MPI_Finalize();
}

TEST(MPICOMM, commnmsg) {
    int stat;
    MPI_Initialized(&stat);
    if (!stat)
        MPI_Init(nullptr, nullptr);
    communication::testing::mpi_registry_mock mock_handle(MPI_COMM_WORLD);
    communication::testing::MPIComm_tester mpic("", new utils::Address("51000,50000"), RECV);
    mpi_registry_t *tempmpi = mpic.getHandle();
    mpic.setHandle(&mock_handle);
    EXPECT_EQ(mpic.comm_nmsg(), 0);

    communication::testing::mpi_registry_mock::MPIPROC = 50000;
    EXPECT_EQ(mpic.comm_nmsg(), 1);
    communication::testing::mpi_registry_mock::MPISTATUS = 1;
    EXPECT_EQ(mpic.comm_nmsg(), -1);
    mpic.setHandle(tempmpi);
    //MPI_Finalize();
}

TEST(MPICOMM, send) {
    int stat;
    MPI_Initialized(&stat);
    if (!stat)
        MPI_Init(nullptr, nullptr);
    communication::testing::mpi_registry_mock mock_handle(MPI_COMM_WORLD);
    communication::testing::MPIComm_tester mpic("", new utils::Address("51000,50000"), SEND);
    mpi_registry_t *tempmpi = mpic.getHandle();
    mpic.setHandle(nullptr);

    EXPECT_EQ(mpic.send("hello", 6), -1);
    mpic.setHandle(&mock_handle);
    EXPECT_EQ(mpic.send("Hello", 6), 0);
    mpic.setHandle(tempmpi);
}

TEST(MPICOMM, recv) {
    int stat;
    MPI_Initialized(&stat);
    if (!stat)
        MPI_Init(nullptr, nullptr);
    communication::testing::mpi_registry_mock mock_handle(MPI_COMM_WORLD);
    communication::testing::MPIComm_tester mpic("", new utils::Address("51000,50000"), RECV);
    mpi_registry_t *tempmpi = mpic.getHandle();
    mpic.setHandle(&mock_handle);
    char* data = (char*)malloc(sizeof(char) * 1);
    size_t len = 1;
    EXPECT_EQ(mpic.recv(data, len, false), -msg.size());
    communication::testing::mpi_registry_mock::MPISTATUS = 2;
    EXPECT_EQ(mpic.recv(data, len, true), -1);
    communication::testing::mpi_registry_mock::MPISTATUS = 0;
    EXPECT_EQ(mpic.recv(data, len, true), msg.size());
    mpic.setHandle(tempmpi);
}

TEST(MPICOMM, regclone) {
    int stat;
    MPI_Initialized(&stat);
    if (!stat)
        MPI_Init(nullptr, nullptr);
    mpi_registry_t mpir(MPI_COMM_WORLD);
    mpir.procs.push_back(1);
    mpi_registry_t mpir2 = mpir.Clone();
    EXPECT_NE(mpir.procs, mpir2.procs);
}
#endif
