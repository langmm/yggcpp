#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/IPCComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include <dlfcn.h>


using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;
TEST(IPCComm, constructor) {
    IPCComm ipc;
    std::string name = "";
    IPCComm ipc2(name, nullptr, SEND);
    EXPECT_TRUE(ipc2.getName().find("tempnewIPC") != std::string::npos);

    utils::Address *adr = new utils::Address("this.is.a.test");
    IPCComm ipc3(name, adr, RECV);

    utils::Address *adr2 = new utils::Address("12345");
    name = "TestName";
    IPCComm ipc4(name, adr2, SEND);

    utils::Address *adr3 = new utils::Address("this.is.a.test");
    EXPECT_THROW(IPCComm ipc5(name, adr3, SEND), std::runtime_error);

    void *handle = dlopen(SUBLIB, RTLD_LAZY);
    void *original_func = nullptr;
    if (!handle)
        EXPECT_TRUE(false);
    original_func = ELFHOOK(msgget)
    EXPECT_NE(original_func, ((void*)0));
    name = "";
    EXPECT_THROW(IPCComm ipc5(name, nullptr, SEND), std::runtime_error);

    ELFREVERT(msgget, original_func)
    dlclose(handle);

}

TEST(IPCComm, send) {
    std::string message = "Hello world";
    std::string name = "";
    SENDCOUNT = 0;

    IPCComm ipc(name, nullptr, RECV);
    int res = ipc.send(message.c_str(), message.size());
    EXPECT_EQ(res, 0);
    void *handle = dlopen(SUBLIB, RTLD_LAZY);
    void *original_func = nullptr;
    void *original_func2 = nullptr;
    //void *sym = dlsym(handle, "msgsnd");
    if (!handle)
        EXPECT_TRUE(false);
    std::string data = "abcdef12345";
    utils::Address *adr2 = new utils::Address("12345678");
    IPCComm ipc2(data, adr2, SEND);
    RETVAL = -1;
    original_func = ELFHOOK(msgsnd)
    EXPECT_NE(original_func, ((void*)0));

    res = ipc2.send(data.c_str(), data.size());
    EXPECT_EQ(SENDCOUNT, 1);
    EXPECT_EQ(res, RETVAL);
    RETVAL = -2;
    original_func2 = ELFHOOK(msgctl)
    EXPECT_NE(original_func2, ((void*)0));
    res = ipc2.send(data.c_str(), data.size());
    EXPECT_EQ(res, RETVAL-1);
    ELFREVERT(msgsnd, original_func)
    ELFREVERT(msgctl, original_func2)
    dlclose(handle);
}


TEST(IPCComm, commnmsg) {
    std::string name = "Comm_nsg_test";
    IPCComm ipc(name, new utils::Address("9876"), SEND);
    int res = ipc.comm_nmsg();

    void *handle = dlopen(SUBLIB, RTLD_LAZY);
    void *original_func = nullptr;

    EXPECT_EQ(res, 0);
    ELFHOOK(msgctl);
    RETVAL = -2;
    res = ipc.comm_nmsg();
    EXPECT_EQ(res, 10000);
    res = ipc.comm_nmsg();
    EXPECT_EQ(res, 0);
    ELFREVERT(msgctl, original_func)
    dlclose(handle);
}

TEST(IPCComm, sendLarge) {
    RETVAL = 0;
    SENDCOUNT = 0;
    std::string name = "SendTester";
    void *handle = dlopen(SUBLIB, RTLD_LAZY);
    void *original_func = nullptr;

    original_func = ELFHOOK(msgsnd);
    EXPECT_NE(original_func, ((void*) 0));
    IPCComm ipc(name, new utils::Address("2468"), SEND);
    std::string msg(YGG_MSG_MAX - 1, 'A');
    EXPECT_EQ(ipc.send(msg), 0);
    EXPECT_EQ(SENDCOUNT, 1);
    SENDCOUNT = 0;
    std::string longmsg(YGG_MSG_MAX * 3 + 20, 'B');
    EXPECT_EQ(ipc.send(longmsg), 0);
    EXPECT_EQ(SENDCOUNT, 5);

    RETVAL = -1;
    EXPECT_EQ(ipc.send(longmsg), -1);

    ELFREVERT(msgsnd, original_func);
    dlclose(handle);

}

TEST(IPCComm, recv) {
    std::string name = "SendTester";
    void *handle = dlopen(SUBLIB, RTLD_LAZY);
    void *original_func = nullptr;
    void* original_func2 = nullptr;
    RETVAL = 0;
    original_func = ELFHOOK(msgrcv);
    EXPECT_NE(original_func, ((void*) 0));
    IPCComm ipc(name, new utils::Address("13579"), RECV);
    char* data = (char*)malloc(sizeof(char));
    size_t len = 1;
    long res = ipc.recv(data, len, false);
    EXPECT_EQ(res, -11);
    original_func2 = ELFHOOK(realloc);
    res = ipc.recv(data, len, true);
    EXPECT_EQ(res, -1);
    ELFREVERT(realloc, original_func2);
    res = ipc.recv(data, len, true);
    EXPECT_EQ(res, 11);
    res = ipc.recv(data, len, true);
    EXPECT_EQ(res, 11);
    RETVAL = -1;
    res = ipc.recv(data, len, true);
    EXPECT_EQ(res, -1);

    ELFREVERT(msgrcv, original_func);
    free(data);
    dlclose(handle);
    int i = 7;
}