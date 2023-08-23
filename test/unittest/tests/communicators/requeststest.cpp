#include "../../unittest.hpp"
#include "communicators/Requests.hpp"
#include "utils/serialization.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"

using namespace communication::communicator;
using namespace communication::utils;

TEST(RequestList, Empty) {
  RequestList req(RECV);
  EXPECT_EQ(req.hasRequest("invalid"), -1);
  EXPECT_EQ(req.hasPartner("invalid"), -1);
  EXPECT_EQ(req.hasComm("invalid"), -1);
  EXPECT_EQ(req.popRequestServer(), -1);
  EXPECT_FALSE(req.lastComm());
  EXPECT_THROW(req.activeRequestClient(), std::exception);
  // EXPECT_EQ(req.activeRequestClient(), "");
  EXPECT_FALSE(req.isComplete("invalid"));
  EXPECT_FALSE(req.signonSent());
  Header header;
  header.initMeta();
  header.SetMetaString("request_id", "invalid");
  // EXPECT_EQ(req.popRequestClient(header), -1);
  EXPECT_EQ(req.getRequestClient("invalid", header), -1);
  EXPECT_TRUE(req.partnerSignoff(header));
}

TEST(RequestList, Request) {
  RequestList client(RECV);
  RequestList server(SEND);
  Header header;
  header.initMeta();
  header.SetMetaString("model", "model");
  // Client
  EXPECT_EQ(client.addRequestClient(header), 0);
  std::string req_id(header.GetMetaString("request_id"));
  EXPECT_EQ(client.addRequestClient(header, header.GetMetaString("request_id")), -1);
  EXPECT_EQ(client.hasRequest(header.GetMetaString("request_id")), 0);
  EXPECT_EQ(client.hasComm(header.GetMetaString("response_address")), 0);
  EXPECT_EQ(client.addRequestClient(header), 1);
  EXPECT_EQ(client.hasRequest(header.GetMetaString("request_id")), 1);
  EXPECT_EQ(client.hasComm(header.GetMetaString("response_address")), 0);
  // Server
  EXPECT_EQ(server.addRequestServer(header), 0);
  EXPECT_EQ(server.hasRequest(header.GetMetaString("request_id")), 0);
  EXPECT_EQ(server.hasComm(header.GetMetaString("response_address")), 0);
  EXPECT_EQ(server.hasPartner(header.GetMetaString("model")), 0);
  // Pop
  EXPECT_EQ(server.popRequestServer(), 1);
  EXPECT_EQ(server.hasRequest(header.GetMetaString("request_id")), -1);
  // EXPECT_EQ(client.popRequestClient(header), -1);
  EXPECT_EQ(client.getRequestClient(header.GetMetaString("request_id"),
				    header), -1);
  client.requests.clear();
  EXPECT_EQ(client.hasRequest(header.GetMetaString("request_id")), -1);
  EXPECT_TRUE(client.requests.empty());
  EXPECT_FALSE(client.comms.empty());
  EXPECT_TRUE(client.lastComm());
}

TEST(RequestList, Response) {
  RequestList client(RECV);
  RequestList server(SEND);
  Header header;
  header.initMeta();
  header.SetMetaString("model", "model");
  std::string request_id;
  header.SetMetaID("request_id", request_id);
  EXPECT_EQ(server.addResponseServer(header), -1);
  EXPECT_EQ(client.addResponseClient(header), -1);
  EXPECT_EQ(client.addRequestClient(header), 0);
  EXPECT_EQ(server.addRequestServer(header), 0);
  EXPECT_EQ(server.addResponseServer(header), 0);
  EXPECT_EQ(server.addResponseServer(header), -1);
  header.Display();
  Header header2; // Use header copy as addResponseClient consumes header
  header2.CopyFrom(header);
  header2.Display();
  EXPECT_EQ(client.addResponseClient(header2), 0); // copy moved
  EXPECT_EQ(client.addResponseClient(header), -1);
  server.Display();
  client.Display();
  request_id.assign(header.GetMetaString("request_id"));
  EXPECT_TRUE(client.isComplete(request_id));
  EXPECT_EQ(client.getRequestClient(request_id, header), 1);
  EXPECT_EQ(client.hasRequest(request_id), 0);
  EXPECT_EQ(server.popRequestServer(), 1);
  EXPECT_EQ(server.hasRequest(request_id), -1);
  EXPECT_EQ(client.getRequestClient(request_id, header, true), 1);
  // EXPECT_EQ(client.popRequestClient(header), 1);
  EXPECT_EQ(client.hasRequest(request_id), -1);
}

TEST(RequestList, Signon) {
  RequestList client(RECV);
  RequestList server(SEND);
  Header header;
  header.initMeta();
  header.SetMetaString("model", "model1");
  header.flags |= HEAD_FLAG_CLIENT_SIGNON;
  EXPECT_EQ(client.addRequestClient(header), 0);
  EXPECT_EQ(client.addRequestClient(header), 0);
  EXPECT_EQ(client.hasRequest(header.GetMetaString("request_id")), 0);
  EXPECT_EQ(client.hasComm(header.GetMetaString("response_address")), 0);
  EXPECT_TRUE(client.signonSent());
  EXPECT_EQ(server.addRequestServer(header), 0);
  EXPECT_EQ(server.addResponseServer(header), 0);
  Header header_cpy;
  header_cpy.CopyFrom(header);
  EXPECT_EQ(client.addResponseClient(header_cpy), 0);
  // Add a second client
  RequestList client2(RECV);
  Header header2;
  header2.initMeta();
  header2.SetMetaString("model", "model2");
  header2.flags |= HEAD_FLAG_CLIENT_SIGNON;
  EXPECT_EQ(client2.addRequestClient(header2), 0);
  EXPECT_EQ(server.addRequestServer(header2), 1);
  EXPECT_TRUE(server.partnerSignoff(header));
  header.flags |= HEAD_FLAG_EOF;
  header2.flags |= HEAD_FLAG_EOF;
  EXPECT_FALSE(server.partnerSignoff(header));
  EXPECT_TRUE(server.partnerSignoff(header2));
}
