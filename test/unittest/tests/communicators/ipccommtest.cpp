#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/IPCComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include <dlfcn.h>
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
    IPCComm_tester ipc4(name, adr2, SEND);

    utils::Address *adr3 = new utils::Address("this.is.a.test");
    EXPECT_THROW(IPCComm_tester ipc5(name, adr3, SEND), std::runtime_error);

#ifdef ELF_AVAILABLE
    ELF_BEGIN;
    ELF_BEGIN_F(msgget);
    name = "";
    EXPECT_THROW(IPCComm_tester ipc5(name, nullptr, SEND), std::runtime_error);
    ELF_END_F(msgget);
    ELF_END;
#endif // ELF_AVAILABLE
}

TEST(IPCComm, send) {
    std::string message = "Hello world";
    std::string name = "";
    SENDCOUNT = 0;

    IPCComm_tester ipc(name, nullptr, RECV);
    int res = ipc.send(message.c_str(), message.size());
    EXPECT_GT(res, 0);
#ifdef ELF_AVAILABLE
    ELF_BEGIN;
    std::string data = "abcdef12345";
    utils::Address *adr2 = new utils::Address("12345678");
    IPCComm_tester ipc2(data, adr2, SEND);
    // Replace msgsnd so that send fails and checks queue status
    ELF_BEGIN_F_RET(msgsnd, -1);
    res = ipc2.send(data.c_str(), data.size());
    EXPECT_EQ(SENDCOUNT, 1);
    EXPECT_EQ(res, RETVAL);
    // Replace msgctl so that when queue status is checked, it fails
    ELF_BEGIN_F_RET(msgctl, -2);
    res = ipc2.send(data.c_str(), data.size());
    EXPECT_EQ(res, RETVAL-1);
    // Restore
    ELF_END_F(msgsnd);
    ELF_END_F(msgctl);
    ELF_END;
#endif // ELF_AVAILABLE
}


TEST(IPCComm, commnmsg) {
    std::string name = "Comm_nsg_test";
    IPCComm_tester ipc(name, new utils::Address("9876"), SEND);
    int res = ipc.comm_nmsg();
    EXPECT_EQ(res, 0);

#ifdef ELF_AVAILABLE
    ELF_BEGIN;
    // Replace msgctl to test failure in comm_nmsg
    ELF_BEGIN_F_RET(msgctl, -2);
    res = ipc.comm_nmsg();
    EXPECT_EQ(res, 10000);
    res = ipc.comm_nmsg();
    EXPECT_EQ(res, 0);
    ELF_END_F(msgctl);
    ELF_END;
#endif // ELF_AVAILABLE
}

TEST(IPCComm, sendLarge) {
#ifdef ELF_AVAILABLE
    RETVAL = 0;
    SENDCOUNT = 0;
    std::string name = "SendTester";
    ELF_BEGIN;
    // Replace msgsnd to test failure on long message?
    ELF_BEGIN_F_RET(msgsnd, 0);
    IPCComm_tester ipc(name, new utils::Address("2468"), SEND);
    std::string msg(ipc.maxMsgSize - 1, 'A');
    EXPECT_GT(ipc.send(msg), 0);
    EXPECT_EQ(SENDCOUNT, 2);
    SENDCOUNT = 0;
    std::string longmsg(ipc.maxMsgSize * 3 + 20, 'B');
    EXPECT_EQ(ipc.send(longmsg), 0);
    EXPECT_EQ(SENDCOUNT, 5);

    RETVAL = -1;
    EXPECT_EQ(ipc.send(longmsg), -1);

    ELF_END_F(msgsnd);
    ELF_END;
#endif // ELF_AVAILABLE
}

TEST(IPCComm, recv) {
#ifdef ELF_AVAILABLE
    std::string name = "SendTester";
    ELF_BEGIN;
    // Replace msgrcv to test different size messages
    ELF_BEGIN_F_RET(msgrcv, 0);
    IPCComm_tester ipc(name, new utils::Address("13579"), RECV);
    char* data = (char*)malloc(sizeof(char));
    size_t len = 1;
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
    res = ipc.recv(data, len, true);
    EXPECT_EQ(res, 11);
    RETVAL = -1;
    res = ipc.recv(data, len, true);
    EXPECT_EQ(res, -1);
    ELF_END_F(msgrcv);
    free(data);
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

