#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/ServerComm.hpp"
#include "communicators/ClientComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include <dlfcn.h>

#ifdef COMM_BASE

using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;

namespace communication {
namespace testing {
class ServerComm_tester : public ServerComm {
public:
    ServerComm_tester(const std::string &name = "", utils::Address *address = nullptr) :
            ServerComm(name, address) {}
    int send(const char* data, const size_t &len) {return ServerComm::send(data, len);}
    long recv(char* data, const size_t &len, bool allow_realloc) {return ServerComm::recv(data, len, allow_realloc);}
};
}
}

TEST(ServerComm, constructor) {
    std::string name = "MyComm";
    ServerComm sc(name, nullptr);
    ServerComm sc1("", nullptr);
    ServerComm sc2("", new utils::Address("12345"));
    EXPECT_TRUE(sc2.new_address());
}

TEST(ServerComm, requests) {
    const std::string rq = "Cx159",
            rq1 = "Ml229";
    std::string name = "MyComm";
    ServerComm sc(name, nullptr);
    EXPECT_EQ(sc.has_request(rq), -1);
    sc.add_request(rq, new utils::Address("1.2.3.4"));
    sc.add_request(rq1, new utils::Address("1.2.3.5"));
    int rqnum = sc.has_request(rq1);
    EXPECT_EQ(rqnum, 1);
    EXPECT_EQ(sc.remove_request(rqnum), 0);
    rqnum = sc.has_request(rq1);
    EXPECT_EQ(rqnum, -1);
    EXPECT_EQ(sc.remove_request(rqnum), -1);
    rqnum = sc.has_request(rq);
    EXPECT_EQ(rqnum, 0);
    EXPECT_EQ(sc.remove_request(rqnum), 0);
}

TEST(ServerComm, comm) {
     const std::string rq = "Cx159",
            rq1 = "Ml229";
    std::string name = "MyComm";
    ServerComm sc(name, nullptr);
    auto* adr = new utils::Address("12345");
    EXPECT_EQ(sc.has_comm(adr->address()), -1);
    EXPECT_EQ(sc.has_comm(adr), -1);
    sc.add_comm(adr);
    std::string newadr = "23456";
    sc.add_comm(newadr);
    EXPECT_GE(sc.has_comm(adr), 0);
    EXPECT_GE(sc.has_comm(newadr), 0);

    EXPECT_EQ(sc.get_comm(0), nullptr);
    sc.add_request(rq, new utils::Address("1.2.3.4"));
    sc.add_request(rq1, new utils::Address("1.2.3.5"));
    EXPECT_NE(sc.get_comm(1), nullptr);
    EXPECT_EQ(sc.get_comm(5), nullptr);
    EXPECT_NE(sc.get_comm(-1), nullptr);
}

TEST(SeverComm, send) {
    const std::string rq = "Cx159",
            rq1 = "Ml229";
    std::string msg = "my message";
    std::string name = "MyComm";
    communication::testing::ServerComm_tester sc(name, nullptr);
    EXPECT_EQ(sc.send(msg.c_str(), msg.size()), -1);
    sc.add_request(rq, new utils::Address("1.2.3.4"));
    sc.add_request(rq1, new utils::Address("1.2.3.5"));
    std::string newadr = "23456";
    sc.add_comm(newadr);
    EXPECT_EQ(sc.send(msg.c_str(), msg.size()), -1);
#ifdef ELF_AVAILABLE
#if COMM_BASE == IPC_COMM
    void *handle = dlopen(SUBLIB, RTLD_LAZY);
    void *original_func = nullptr;
    RETVAL = 5;
    original_func = ELFHOOK(msgsnd);
    EXPECT_NE(original_func, ((void*) 0));
    EXPECT_EQ(sc.send(msg.c_str(), msg.size()), -1);
    ELFREVERT(msgsnd, original_func);
    dlclose(handle);

#elif COMM_BASE == ZMQ_COMM
#endif
#endif // ELF_AVAILABLE
}

TEST(ServerComm, recv) {
#ifdef ELF_AVAILABLE
#if COMM_BASE == IPC_COMM
    std::string name = "MyComm";
    char* data = (char*)malloc(sizeof(char));
    size_t len = 1;

    communication::testing::ServerComm_tester sc(name, nullptr);

    void *handle = dlopen(SUBLIB, RTLD_LAZY);
    void *original_func = nullptr;
    void* original_func2 = nullptr;
    RETVAL = 0;
    original_func = ELFHOOK(msgrcv);
    EXPECT_NE(original_func, ((void*) 0));
    EXPECT_LT(sc.recv(data, len, false), -1);
    //EXPECT_GT(sc.recv(data, len, true), 0);

    //ELFREVERT(msgrcv, original_func);
    free(data);
    dlclose(handle);

#elif COMM_BASE == ZMQ_COMM
#endif
#endif // ELF_AVAILABLE
}

#endif
