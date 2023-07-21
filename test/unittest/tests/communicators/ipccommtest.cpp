#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/IPCComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"


using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;

class IPCComm_tester : public IPCComm {
public:
  TESTER_METHODS(IPCComm)
};

#ifdef IPCINSTALLED

COMM_SERI_TEST(IPCComm)

TEST(IPCComm, constructor) {
    IPCComm_tester ipc;
    std::string name = "";
    IPCComm_tester ipc2(name, nullptr, SEND);
    EXPECT_TRUE(ipc2.getName().find("tempnewIPC") != std::string::npos);

    utils::Address *adr = new utils::Address("this.is.a.test");
    IPCComm_tester ipc3(name, adr, RECV);

    utils::Address *adr2 = new utils::Address("12345");
    name = "TestName";
    EXPECT_THROW(IPCComm_tester ipc4(name, adr2, RECV), std::runtime_error);

#ifdef ELF_AVAILABLE
    name = "";
    ELF_BEGIN;
    ELF_BEGIN_F(msgget); // To allow connection to non-existed queue
    RETVAL = 0;
    utils::Address *adr3 = new utils::Address("12345");
    IPCComm_tester ipc5(name, adr3, RECV);
    RETVAL = -1;
    EXPECT_THROW(IPCComm_tester ipc6(name, nullptr, SEND), std::runtime_error);
    ELF_END_F(msgget);
    ELF_END;
#endif // ELF_AVAILABLE
}

TEST(IPCComm, send) {
    std::string message = "Hello world";
    std::string name = "";
    IPCComm_tester ipc(name, nullptr, RECV);
    int res = ipc.send(message.c_str(), message.size());
    EXPECT_GT(res, 0);
#ifdef ELF_AVAILABLE
    ELF_BEGIN;
    ELF_BEGIN_F(msgget);
    std::string data = "abcdef12345";
    utils::Address *adr2 = new utils::Address("12345678");
    IPCComm_tester ipc2(data, adr2, SEND);
    // Replace msgsnd and msgctl so that send fails, but msgctl succeeds
    ELF_REPLACE_SEND_IPC;
    ELF_REPLACE_NMSG_IPC;
    SENDCOUNT = 0;
    RETVAL = -1;
    RETVAL_INC_SEND = 1;
    res = ipc2.send(data.c_str(), data.size());
    EXPECT_GE(res, 0);
    EXPECT_EQ(SENDCOUNT, 1);
    // Failure on msgctl
    RETVAL = -1;
    RETVAL_INC_SEND = 0;
    RETVAL_INC_POLL = 0;
    res = ipc2.send(data.c_str(), data.size());
    EXPECT_EQ(res, -1);
    // Failure on msgsnd after EAGAIN
    RETVAL = -1;
    RETVAL_INC_SEND = -1;
    errno = EAGAIN;
    res = ipc2.send(data.c_str(), data.size());
    EXPECT_EQ(res, -1);
    EXPECT_EQ(SENDCOUNT, 1);
    errno = 0;
    // Restore
    ELF_RESTORE_SEND_IPC;
    ELF_RESTORE_NMSG_IPC;
    ELF_END_F(msgget);
    ELF_END;
#endif // ELF_AVAILABLE
}


TEST(IPCComm, commnmsg) {
    std::string name = "Comm_nsg_test";
    IPCComm_tester ipc(name, NULL, SEND);
    int res = ipc.comm_nmsg();
    EXPECT_EQ(res, 0);

#ifdef ELF_AVAILABLE
    ELF_BEGIN;
    // Replace msgctl to test failure in comm_nmsg
    ELF_REPLACE_NMSG_IPC;
    ELF_SET_SUCCESS;
    res = ipc.comm_nmsg();
    EXPECT_EQ(res, 10000);
    ELF_SET_FAILURE;
    res = ipc.comm_nmsg();
    EXPECT_EQ(res, 0);
    ELF_RESTORE_NMSG_IPC;
    ELF_END;
#endif // ELF_AVAILABLE
}

TEST(IPCComm, sendLarge) {
#ifdef ELF_AVAILABLE
    std::string name = "SendTester";
    ELF_BEGIN;
    ELF_BEGIN_F(msgget); // To allow connection to non-existed queue
    // Replace msgsnd to test sending long message
    ELF_REPLACE_SEND_IPC;
    ELF_SET_SUCCESS;
    IPCComm_tester ipc(name, new utils::Address("2468"), SEND);
    std::string msg(ipc.getMaxMsgSize() - 1, 'A');
    EXPECT_GT(ipc.send(msg), 0);
    EXPECT_EQ(SENDCOUNT, 2);
    SENDCOUNT = 0;
    std::string longmsg(ipc.getMaxMsgSize() * 3 + 20, 'B');
    EXPECT_GT(ipc.send(longmsg), 0);
    EXPECT_EQ(SENDCOUNT, 4);

    // Failure on creation of temp communicator
    ELF_SET_FAILURE;
    EXPECT_EQ(ipc.send(longmsg), -1);

    // Failure on send
    ELF_END_F(msgget);
    EXPECT_EQ(ipc.send(longmsg), -1);

    ELF_RESTORE_SEND_IPC;
    ELF_END;
#endif // ELF_AVAILABLE
}

TEST(IPCComm, recv) {
#ifdef ELF_AVAILABLE
    std::string name = "SendTester";
    ELF_BEGIN;
    ELF_BEGIN_F(msgget); // To allow connection to non-existed queue
    ELF_SET_SUCCESS;
    IPCComm_tester ipc(name, new utils::Address("13579"), RECV);
    // Replace msgrcv to test different size messages
    ELF_REPLACE_RECV_IPC;
    ELF_REPLACE_NMSG_IPC;
    char* data = (char*)malloc(sizeof(char));
    size_t len = 1;
    // Failure to realloc
    long res = ipc.recv(data, len, false);
    EXPECT_EQ(res, -11);
    // Replace realloc to test failure to realloc
    ELF_BEGIN_F(realloc);
    res = ipc.recv(data, len, true);
    EXPECT_EQ(res, -1);
    ELF_END_F(realloc);
    // Test successful receive
    res = ipc.recv(data, len, true);
    EXPECT_EQ(res, 11);
    // Test failure in receive
    RETVAL = 0;
    RETVAL_INC_POLL = -2;
    RETVAL_INC_RECV = 0;
    res = ipc.recv(data, len, true);
    EXPECT_EQ(res, -1);
    // Retry in msgrcv
    RETVAL = 0;
    RETVAL_INC_POLL = -1;
    RETVAL_INC_RECV = 1;
    errno = ENOMSG;
    EXPECT_EQ(ipc.recv(data, len, true), 11);
    errno = 0;
    free(data);
    // Restore methods
    ELF_END_F(msgget);
    ELF_RESTORE_RECV_IPC;
    ELF_RESTORE_NMSG_IPC;
    ELF_END;
#endif // ELF_AVAILABLE
}

#else // IPCINSTALLED

TEST(IPCComm, constructor) {
    EXPECT_THROW(IPCComm_tester ipc, std::exception);
    std::string name = "";
    EXPECT_THROW(IPCComm_tester ipc2(name, nullptr, SEND), std::exception);
}

#endif // IPCINSTALLED

