#include "../../unittest.hpp"
#include "utils/Address.hpp"
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
class ClientComm_tester : public ClientComm {
public:
    ClientComm_tester(const std::string &name = "", utils::Address *address = nullptr) :
            ClientComm(name, address) {}
    int send(const char* data, const size_t &len) {return ClientComm::send(data, len);}
    long recv(char* data, const size_t &len, bool allow_realloc) {return ClientComm::recv(data, len, allow_realloc);}
};
}
}

TEST(ClientComm, constructor) {
    std::string name = "MyComm";
    ClientComm cc(name, nullptr);
    ClientComm cc1("", nullptr);
    ClientComm cc2("", new utils::Address("12345"));
    EXPECT_TRUE(cc2.new_address());
}

TEST(ClientComm, requests) {
    const std::string rq = "Cx159",
                      rq1 = "Ml229";
    std::string name = "MyComm";
    ClientComm cc(name, nullptr);
    EXPECT_EQ(cc.has_request(rq), -1);
    cc.add_request(rq);
    cc.add_request(rq1);
    EXPECT_EQ(cc.has_request(rq1), 1);
    EXPECT_EQ(cc.remove_request(rq), 0);
    EXPECT_EQ(cc.has_request(rq1), 0);
    EXPECT_EQ(cc.remove_request(rq), 0);
    EXPECT_EQ(cc.has_request(rq1), 0);
}

TEST(ClientComm, responses) {
    const std::string rq = "Cx159",
                      rq1 = "Ml229";
    const std::string rs = "Cx159",
                      rs1 = "Ml229";
    std::string name = "MyComm";
    ClientComm cc(name, nullptr);
    EXPECT_EQ(cc.has_response(rs), -1);
    std::string res1 = "This is a response";
    cc.add_response(rs, res1.c_str(), res1.size());
    EXPECT_EQ(cc.has_response(rs), -1);
    cc.add_request(rq);
    cc.add_response(rs, res1.c_str(), res1.size());
    EXPECT_EQ(cc.has_response(rs), 0);
    cc.remove_request(rq);
    char* data = (char*)malloc(sizeof(char));
    size_t mlen = 1;
    EXPECT_EQ(cc.pop_response(rs, data, mlen, false), -1);
    cc.add_request(rq);
    cc.add_request(rq1);
    cc.add_response(rs1, res1.c_str(), res1.size());
    EXPECT_EQ(cc.pop_response(rs1, data, mlen, false), -res1.size());
    EXPECT_EQ(cc.pop_response(rs1, data, mlen, true), res1.size());
}

TEST(ClientComm, recv) {
    std::string name = "MyComm";
    communication::testing::ClientComm_tester cc(name, nullptr);
    char* data = (char*)malloc(sizeof(char));
    size_t len = 1;
    EXPECT_EQ(cc.recv(data, len, false), -1);
    std::string rq = "Cx159",
                rq1 = "Ml229";
    std::string rs = "Cx159",
                rs1 = "Ml229";
    std::string res1 = "This is a response";
    cc.add_request(rq);
    cc.add_response(rs, res1.c_str(), res1.size());
#if COMM_BASE == IPC_COMM
    void *handle = dlopen(SUBLIB, RTLD_LAZY);
    void *original_func = nullptr;
    void* original_func2 = nullptr;
    RETVAL = -2;
    original_func = ELFHOOK(msgrcv);
    EXPECT_NE(original_func, ((void*) 0));
    EXPECT_EQ(cc.recv(data, len, false), -res1.size());
    EXPECT_EQ(cc.recv(data, len, true), res1.size());

    ELFREVERT(msgrcv, original_func);
    free(data);
    dlclose(handle);

#elif COMM_BASE == ZMQ_COMM
#endif
}

#endif