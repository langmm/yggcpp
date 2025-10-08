#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/RESTComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"
#include <stdio.h>


using namespace YggInterface;
using namespace YggInterface::communicator;
using namespace YggInterface::mock;

class RESTComm_tester : public RESTComm {
public:
  TESTER_METHODS(RESTComm)
};

#ifdef RESTINSTALLED

#ifndef _MSC_VER // No popen with MSVC

class RunFlaskApp {
private:
  RunFlaskApp(const RunFlaskApp&) = delete;
  RunFlaskApp& operator=(const RunFlaskApp&) = delete;
public:
  RunFlaskApp() : app(NULL), curl(NULL), cmd("python example_app.py"),
		  child_pid(-1) {
    app = popen(cmd.c_str(), "r");
    if (app == NULL) {
      throw std::runtime_error("Flask app could not be started");
    }
    curl = curl_easy_init();
    usleep(100000);
    while (!post("startup")) {
      usleep(10000);
    }
    // if (!find_child())
    //   throw std::runtime_error("Could not locate child process");
  }
  ~RunFlaskApp() {
    if (app) {
      post("shutdown");
      char buffer[80];
      while (fgets(&(buffer[0]), 80, app) != NULL) {
	std::cerr << buffer;
      }
      // int count = 0;
      // while (find_child() && (count < 10)) {
      // 	kill(child_pid, SIGKILL);
      // 	count++;
      // }
      // if (find_child())
      // 	std::cerr << "Child still alive" << std::endl;
      // 	// throw std::runtime_error("Child still alive");
      pclose(app);
    }
    if (curl) {
      curl_easy_cleanup(curl);
    }
  }
  bool find_child() {
    child_pid = -1;
    std::string grep_cmd = "ps | grep \"" + cmd + "\" | grep -v grep | grep -v \"sh -c \" | sed \'s/^ *//\' | sed \'s/ .*$//\'";
    FILE* grep_ps = popen(grep_cmd.c_str(), "r");
    if (grep_ps == NULL) {
      throw std::runtime_error("Could not launch grep for child process");
    }
    int found = fscanf(grep_ps, "%d", &child_pid);
    pclose(grep_ps);
    return (found == 1);
  }
  bool post(const std::string& addr) {
    std::string address = "http://localhost:5000/" + addr;
#define CHECK_ERROR(method)						\
    {									\
      CURLcode x = method;						\
      if (x != CURLE_OK) {						\
	std::cerr << "CURL ERROR: " << curl_easy_strerror(x) << std::endl; \
	return false;							\
      }									\
    }
    CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_URL, address.c_str()));
    CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_HTTPGET, 1L));
    CHECK_ERROR(curl_easy_perform(curl));
#undef CHECK_ERROR
    return true;
  }
    
  FILE* app;
  CURL* curl;
  std::string cmd;
  int child_pid;
};

COMM_SERI_TEST_BASE(RESTComm, RunFlaskApp app);

#endif // _MSC_VER

#else // RESTINSTALLED

TEST(RESTComm, constructor) {
    EXPECT_THROW(RESTComm_tester ipc(""), std::exception);
    std::string name = "";
    EXPECT_THROW(RESTComm_tester ipc2(name, SEND), std::exception);
}

#endif // RESTINSTALLED
