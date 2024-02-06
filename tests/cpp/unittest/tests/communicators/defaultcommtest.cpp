#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "utils/tools.hpp"
#include "communicators/DefaultComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"

using namespace YggInterface;
using namespace YggInterface::communicator;
using namespace YggInterface::mock;

TEST(DefaultCommu, checkTypeErrors) {
  DefaultComm x("", SEND);
  EXPECT_TRUE(x.addSchema("{\"type\": \"boolean\"}"));
  {
    double data = 5.0;
    EXPECT_EQ(x.sendVar(data), -1);
    EXPECT_EQ(x.recvVar(data), -1);
  }
  {
    std::string data;
    EXPECT_EQ(x.sendVar(data), -1);
    EXPECT_EQ(x.recvVar(data), -1);
  }
  {
    rapidjson::Document data;
    data.Set(5.0);
    EXPECT_EQ(x.sendVar(data), -1);
    EXPECT_EQ(x.recvVar(data), -1);
  }
  {
    rapidjson::Ply data;
    EXPECT_EQ(x.sendVar(data), -1);
    EXPECT_EQ(x.recvVar(data), -1);
  }
  {
    rapidjson::ObjWavefront data;
    EXPECT_EQ(x.sendVar(data), -1);
    EXPECT_EQ(x.recvVar(data), -1);
  }
}

TEST(DefaultCommu, seriErrors) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress().c_str());
  DefaultComm rComm("", addr, RECV);
  int a = 0, b = 0;
  EXPECT_EQ(sComm.send(2, 1, 1), -1); // No schema to parse variable arguments
  EXPECT_GE(sComm.send("hello", 5), 0);
  EXPECT_EQ(rComm.recvVar(a, b), -1); // Type mismatch
}

TEST(DefaultCommu, workerErrors) {
  DefaultComm sComm("", SEND);
  EXPECT_FALSE(sComm.getWorkers().setRequest(nullptr, "invalid"));
  EXPECT_FALSE(sComm.getWorkers().setResponse("invalid"));
  utils::Address addr(sComm.getAddress().c_str());
  EXPECT_EQ(sComm.getWorkers().get(nullptr, RECV, addr), nullptr);
}

TEST(DefaultCommu, send_dict) {
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_send.Parse("[\"a\", 1, 5.0]");
  msg_expc.Parse("{\"a\": \"a\", \"b\": 1, \"c\": 5.0}");
  std::vector<std::string> key_order = {"a", "b", "c"};
  std::vector<std::string> invalid_key_order = {"a", "b"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_LT(sComm.send_dict(msg_send, invalid_key_order), 0);
  EXPECT_GT(sComm.send_dict(msg_send, key_order), 0);
  EXPECT_GT(rComm.recv(msg_recv), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, recv_dict) {
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_send.Parse("[\"a\", 1, 5.0]");
  msg_expc.Parse("{\"a\": \"a\", \"b\": 1, \"c\": 5.0}");
  std::vector<std::string> key_order = {"a", "b", "c"};
  std::vector<std::string> invalid_key_order = {"a", "b"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_GT(sComm.send(msg_send), 0);
  EXPECT_LT(rComm.recv_dict(msg_recv, invalid_key_order), 0);
  EXPECT_GT(sComm.send(msg_send), 0);
  EXPECT_GT(rComm.recv_dict(msg_recv, key_order), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, send_dict_default) {
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_send.Parse("[\"a\", 1, 5.0]");
  msg_expc.Parse("{\"f0\": \"a\", \"f1\": 1, \"f2\": 5.0}");
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_GT(sComm.send_dict(msg_send), 0);
  EXPECT_GT(rComm.recv(msg_recv), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, send_dict_field_names) {
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_send.Parse("[\"a\", 1, 5.0]");
  msg_expc.Parse("{\"a\": \"a\", \"b\": 1, \"c\": 5.0}");
  std::vector<std::string> key_order = {"a", "b", "c"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_TRUE(sComm.getMetadata().set_field_names(key_order));
  EXPECT_GT(sComm.send_dict(msg_send), 0);
  EXPECT_GT(rComm.recv(msg_recv), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, recv_dict_field_names) {
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_send.Parse("[\"a\", 1, 5.0]");
  msg_expc.Parse("{\"a\": \"a\", \"b\": 1, \"c\": 5.0}");
  std::vector<std::string> key_order = {"a", "b", "c"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_TRUE(rComm.getMetadata().set_field_names(key_order));
  EXPECT_GT(sComm.send(msg_send), 0);
  EXPECT_GT(rComm.recv_dict(msg_recv), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, send_dict_array) {
  int arr[2][2][3] = {{{0, 1, 2},
		       {3, 4, 5}},
		      {{6, 7, 8},
		       {9, 10, 11}}};
  int arr_a[2][2] = {{0, 3}, {6, 9}};
  int arr_b[2][2] = {{1, 4}, {7, 10}};
  int arr_c[2][2] = {{2, 5}, {8, 11}};
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_send.SetNDArray(arr, msg_send.GetAllocator());
  msg_expc.SetObject();
  rapidjson::Value val_a, val_b, val_c;
  val_a.SetNDArray(arr_a, msg_expc.GetAllocator());
  val_b.SetNDArray(arr_b, msg_expc.GetAllocator());
  val_c.SetNDArray(arr_c, msg_expc.GetAllocator());
  msg_expc.AddMember("a", val_a, msg_expc.GetAllocator());
  msg_expc.AddMember("b", val_b, msg_expc.GetAllocator());
  msg_expc.AddMember("c", val_c, msg_expc.GetAllocator());
  std::vector<std::string> key_order = {"a", "b", "c"};
  std::vector<std::string> invalid_key_order = {"a", "b"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_LT(rComm.send_dict(msg_send, invalid_key_order, 2), 0);
  EXPECT_LT(sComm.send_dict(msg_send, key_order, 3), 0);
  EXPECT_GT(sComm.send_dict(msg_send, key_order, 2), 0);
  EXPECT_GT(rComm.recv(msg_recv), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, recv_dict_array) {
  int arr[2][2][3] = {{{0, 1, 2},
		       {3, 4, 5}},
		      {{6, 7, 8},
		       {9, 10, 11}}};
  int arr_a[2][2] = {{0, 3}, {6, 9}};
  int arr_b[2][2] = {{1, 4}, {7, 10}};
  int arr_c[2][2] = {{2, 5}, {8, 11}};
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_send.SetNDArray(arr, msg_send.GetAllocator());
  msg_expc.SetObject();
  rapidjson::Value val_a, val_b, val_c;
  val_a.SetNDArray(arr_a, msg_expc.GetAllocator());
  val_b.SetNDArray(arr_b, msg_expc.GetAllocator());
  val_c.SetNDArray(arr_c, msg_expc.GetAllocator());
  msg_expc.AddMember("a", val_a, msg_expc.GetAllocator());
  msg_expc.AddMember("b", val_b, msg_expc.GetAllocator());
  msg_expc.AddMember("c", val_c, msg_expc.GetAllocator());
  std::vector<std::string> key_order = {"a", "b", "c"};
  std::vector<std::string> invalid_key_order = {"a", "b"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_GT(sComm.send(msg_send), 0);
  EXPECT_LT(rComm.recv_dict(msg_recv, invalid_key_order, 2), 0);
  EXPECT_GT(sComm.send(msg_send), 0);
  EXPECT_GT(rComm.recv_dict(msg_recv, key_order, 2), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, send_array) {
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_expc.Parse("[\"a\", 1, 5.0]");
  msg_send.Parse("{\"a\": \"a\", \"b\": 1, \"c\": 5.0}");
  std::vector<std::string> key_order = {"a", "b", "c"};
  std::vector<std::string> invalid_key_order = {"a", "b", "invalid"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_LT(sComm.send_array(msg_send, invalid_key_order), 0);
  EXPECT_GT(sComm.send_array(msg_send, key_order), 0);
  EXPECT_GT(rComm.recv(msg_recv), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, recv_array) {
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_expc.Parse("[\"a\", 1, 5.0]");
  msg_send.Parse("{\"a\": \"a\", \"b\": 1, \"c\": 5.0}");
  std::vector<std::string> key_order = {"a", "b", "c"};
  std::vector<std::string> invalid_key_order = {"a", "b", "invalid"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_GT(sComm.send(msg_send), 0);
  EXPECT_LT(rComm.recv_array(msg_recv, invalid_key_order), 0);
  EXPECT_GT(sComm.send(msg_send), 0);
  EXPECT_GT(rComm.recv_array(msg_recv, key_order), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, send_array_default) {
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_expc.Parse("[\"a\", 1, 5.0]");
  msg_send.Parse("{\"a\": \"a\", \"b\": 1, \"c\": 5.0}");
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_GT(sComm.send_array(msg_send), 0);
  EXPECT_GT(rComm.recv(msg_recv), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, send_array_field_names) {
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_expc.Parse("[\"a\", 1, 5.0]");
  msg_send.Parse("{\"a\": \"a\", \"b\": 1, \"c\": 5.0}");
  std::vector<std::string> key_order = {"a", "b", "c"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_TRUE(sComm.getMetadata().set_field_names(key_order));
  EXPECT_GT(sComm.send_array(msg_send), 0);
  EXPECT_GT(rComm.recv(msg_recv), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, recv_array_field_names) {
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_expc.Parse("[\"a\", 1, 5.0]");
  msg_send.Parse("{\"a\": \"a\", \"b\": 1, \"c\": 5.0}");
  std::vector<std::string> key_order = {"a", "b", "c"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_TRUE(rComm.getMetadata().set_field_names(key_order));
  EXPECT_GT(sComm.send(msg_send), 0);
  EXPECT_GT(rComm.recv_array(msg_recv), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, send_array_array) {
  int arr[2][2][3] = {{{0, 1, 2},
		       {3, 4, 5}},
		      {{6, 7, 8},
		       {9, 10, 11}}};
  int arr_a[2][2] = {{0, 3}, {6, 9}};
  int arr_b[2][2] = {{1, 4}, {7, 10}};
  int arr_c[2][2] = {{2, 5}, {8, 11}};
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_expc.SetArray();
  msg_expc.Reserve(3, msg_expc.GetAllocator());
  msg_send.SetNDArray(arr, msg_send.GetAllocator());
  rapidjson::Value val_a, val_b, val_c;
  val_a.SetNDArray(arr_a, msg_expc.GetAllocator());
  val_b.SetNDArray(arr_b, msg_expc.GetAllocator());
  val_c.SetNDArray(arr_c, msg_expc.GetAllocator());
  msg_expc.PushBack(val_a, msg_expc.GetAllocator());
  msg_expc.PushBack(val_b, msg_expc.GetAllocator());
  msg_expc.PushBack(val_c, msg_expc.GetAllocator());
  std::vector<std::string> key_order = {"a", "b", "c"};
  std::vector<std::string> invalid_key_order = {"a", "b"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_LT(rComm.send_array(msg_send, invalid_key_order, 2), 0);
  EXPECT_LT(sComm.send_array(msg_send, key_order, 3), 0);
  EXPECT_GT(sComm.send_array(msg_send, key_order, 2), 0);
  EXPECT_GT(rComm.recv(msg_recv), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, recv_array_array) {
  int arr[2][2][3] = {{{0, 1, 2},
		       {3, 4, 5}},
		      {{6, 7, 8},
		       {9, 10, 11}}};
  int arr_a[2][2] = {{0, 3}, {6, 9}};
  int arr_b[2][2] = {{1, 4}, {7, 10}};
  int arr_c[2][2] = {{2, 5}, {8, 11}};
  rapidjson::Document msg_send, msg_recv, msg_expc;
  msg_expc.SetArray();
  msg_expc.Reserve(3, msg_expc.GetAllocator());
  msg_send.SetNDArray(arr, msg_send.GetAllocator());
  rapidjson::Value val_a, val_b, val_c;
  val_a.SetNDArray(arr_a, msg_expc.GetAllocator());
  val_b.SetNDArray(arr_b, msg_expc.GetAllocator());
  val_c.SetNDArray(arr_c, msg_expc.GetAllocator());
  msg_expc.PushBack(val_a, msg_expc.GetAllocator());
  msg_expc.PushBack(val_b, msg_expc.GetAllocator());
  msg_expc.PushBack(val_c, msg_expc.GetAllocator());
  std::vector<std::string> key_order = {"a", "b", "c"};
  std::vector<std::string> invalid_key_order = {"a", "b"};
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  EXPECT_GT(sComm.send(msg_send), 0);
  EXPECT_LT(rComm.recv_array(msg_recv, invalid_key_order, 2), 0);
  EXPECT_GT(sComm.send(msg_send), 0);
  EXPECT_GT(rComm.recv_array(msg_recv, key_order, 2), 0);
  EXPECT_EQ(msg_recv, msg_expc);
}

TEST(DefaultCommu, mismatched_send) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  int msg1_send = 5, msg1_recv = 0;
  std::string msg2_send = "hello";
  EXPECT_GT(sComm.sendVar(msg1_send), 0);
  EXPECT_GT(rComm.recvVar(msg1_recv), 0);
  EXPECT_EQ(msg1_recv, msg1_send);
  EXPECT_EQ(sComm.sendVar(msg2_send), -1);
}

TEST(DefaultCommu, mismatched_recv) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  int msg1_send = 5, msg1_recv = 0;
  std::string msg2_send = "hello";
  EXPECT_GT(sComm.sendVar(msg1_send), 0);
  EXPECT_GT(rComm.recvVar(msg1_recv), 0);
  EXPECT_EQ(msg1_recv, msg1_send);
  sComm.getMetadata().reset();
  EXPECT_GT(sComm.sendVar(msg2_send), 0);
  EXPECT_EQ(rComm.recvVar(msg1_recv), -1);
}

TEST(DefaultCommu, filter_recv) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  rComm.getMetadata().addFilter(example_filter);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_GT(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  int result = -1;
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 0);
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 2);
  EXPECT_EQ(rComm.recvVar(result), -2);
}
TEST(DefaultCommu, filter_send) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  sComm.getMetadata().addFilter(example_filter);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_EQ(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  int result = -1;
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 0);
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 2);
  EXPECT_EQ(rComm.recvVar(result), -2);
}

TEST(DefaultCommu, transform_recv) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  rComm.getMetadata().addTransform(&example_transform);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_GT(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.sendVar(5), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  std::string result = "";
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "0");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "1");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "2");
  EXPECT_EQ(rComm.recvVar(result), -1);
  EXPECT_EQ(rComm.recvVar(result), -2);
}
TEST(DefaultCommu, transform_send) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  sComm.getMetadata().addTransform(&example_transform);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_GT(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_LT(sComm.sendVar(5), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  std::string result = "";
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "0");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "1");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "2");
  EXPECT_EQ(rComm.recvVar(result), -2);
}
// error
TEST(DefaultCommu, filter_recv_error) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  rComm.getMetadata().addFilter(example_filter_error);
  EXPECT_GT(sComm.sendVar(0), 0);
  int result = -1;
  EXPECT_EQ(rComm.recvVar(result), -1);
  EXPECT_EQ(result, -1);
}
TEST(DefaultCommu, filter_send_error) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  sComm.getMetadata().addFilter(example_filter_error);
  EXPECT_EQ(sComm.sendVar(0), -1);
  EXPECT_EQ(rComm.comm_nmsg(), 0);
}

TEST(DefaultCommu, transform_recv_error) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  rComm.getMetadata().addTransform(&example_transform_error);
  EXPECT_GT(sComm.sendVar(0), 0);
  std::string result = "";
  EXPECT_EQ(rComm.recvVar(result), -1);
  EXPECT_EQ(result, "");
}
TEST(DefaultCommu, transform_send_error) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  sComm.getMetadata().addTransform(&example_transform_error);
  EXPECT_EQ(sComm.sendVar(0), -1);
  EXPECT_EQ(rComm.comm_nmsg(), 0);
}


#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
TEST(DefaultCommu, py_filter_recv) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  PyObject* py_filter = utils::import_python_element(
      "example_python", "example_filter");
  rComm.getMetadata().addFilter(py_filter);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_GT(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  int result = -1;
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 0);
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 2);
  EXPECT_EQ(rComm.recvVar(result), -2);
}
TEST(DefaultCommu, py_filter_send) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  PyObject* py_filter = utils::import_python_element(
      "example_python", "example_filter");
  sComm.getMetadata().addFilter(py_filter);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_EQ(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  int result = -1;
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 0);
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 2);
  EXPECT_EQ(rComm.recvVar(result), -2);
}

TEST(DefaultCommu, py_transform_recv) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  PyObject* py_transform = utils::import_python_element(
      "example_python", "example_transform");
  rComm.getMetadata().addTransform(py_transform);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_GT(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  std::string result = "";
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "0");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "1");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "2");
  EXPECT_EQ(rComm.recvVar(result), -2);
}
TEST(DefaultCommu, py_transform_send) {
  DefaultComm sComm("", SEND);
  utils::Address addr(sComm.getAddress());
  DefaultComm rComm("", addr, RECV);
  PyObject* py_transform = utils::import_python_element(
      "example_python", "example_transform");
  sComm.getMetadata().addTransform(py_transform);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_GT(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  std::string result = "";
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "0");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "1");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "2");
  EXPECT_EQ(rComm.recvVar(result), -2);
}
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
