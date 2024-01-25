#include "communicators/RESTComm.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

// TODO: Check that server can be connected

size_t _writeFunction(void *ptr, size_t size, size_t nmemb, std::string* data) {
  data->append((char*) ptr, size * nmemb);
  return size * nmemb;
}
size_t _writeHeader(void *ptr, size_t size, size_t nmemb, YggInterface::utils::Header* data) {
  if (data->on_recv((char*) ptr, size * nmemb) < 0)
    return 0;
  return size * nmemb;
}

RESTConnection::RESTConnection(const std::string logInst, DIRECTION dir,
			       const std::string& nme,
			       const std::string& addr,
			       const std::string& mod) :
  LogBase(), logInst_(logInst), name(nme), address(addr), direction(dir),
  model(mod)
#ifdef RESTINSTALLED
  , curl(NULL)
#endif // RESTINSTALLED
{
  if (init() < 0) {
    throw_error("RESTConnection: Failed to initialize connection");
  }
  for (size_t i = 0; i < name.size(); i++) {
    if (name[i] == ':')
      name[i] = '-';
  }
  if (name.empty())
    name = random_string(4);
}
RESTConnection::~RESTConnection() {
  if (close() < 0) {
    throw_error("RESTConnection: Failed to close connection");
  }
}
#ifdef RESTINSTALLED
#define CHECK_CURL_ERROR(method, context)	\
  {						\
    if (!_check_curl_error(method, context)) {	\
      return -1;				\
    }						\
  }

int RESTConnection::init() {
  if (address.empty()) {
    std::string host, client_id;
    host = "http://localhost:";
    char* port = std::getenv("PORT");
    if (port)
      host += std::string(port);
    else
      host += "5000";
    client_id = random_string(4);
    address = host + "/" + client_id + "/" + model + "/" + name;
  }
#define CHECK_ERROR(method) CHECK_CURL_ERROR(method, "init")
  curl = curl_easy_init();
  if (!curl) {
    log_error() << "init: Failed to create curl pointer" << std::endl;
    return -1;
  }
#undef CHECK_ERROR
  return 0;
}
int RESTConnection::close() {
  if (!curl)
    return 0;
#define CHECK_ERROR(method) CHECK_CURL_ERROR(method, "close")
  curl_easy_reset(curl);
  std::string close_address = address + "/remove";
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_URL, close_address.c_str()));
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_HTTPGET, 1L));
  CHECK_ERROR(curl_easy_perform(curl));
  curl_easy_cleanup(curl);
#undef CHECK_ERROR
  return 0;
}
int RESTConnection::nmsg(DIRECTION) const {
  if (!curl)
    return -1;
#define CHECK_ERROR(method) CHECK_CURL_ERROR(method, "nmsg")
  curl_easy_reset(curl);
  std::string nmsg_address = address + "/size";
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_URL, nmsg_address.c_str()));
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_HTTPGET, 1L));
  std::string response;
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, _writeFunction));
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response));
  CHECK_ERROR(curl_easy_perform(curl));
#undef CHECK_ERROR
  if (response.empty())
    return -1;
  return std::stoi(response);
}
int RESTConnection::send(utils::Header& header) {
  if (!curl)
    return -1;
#define CHECK_ERROR(method) CHECK_CURL_ERROR(method, "send")
  curl_easy_reset(curl);
  struct curl_slist *list;
  list = curl_slist_append(NULL, "Content-Type:");
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_URL, address.c_str()));
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_POST, 1L));
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, header.size_msg));
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_COPYPOSTFIELDS, header.data_msg()));
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_HTTPHEADER, list));
  CHECK_ERROR(curl_easy_perform(curl));
  curl_slist_free_all(list);
#undef CHECK_ERROR
  return 1;
}
long RESTConnection::recv(utils::Header& header) {
  if (!curl)
    return -1;
  size_t old_size = header.size_curr;
#define CHECK_ERROR(method) CHECK_CURL_ERROR(method, "recv")
  curl_easy_reset(curl);
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_URL, address.c_str()));
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_HTTPGET, 1L));
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, _writeHeader));
  CHECK_ERROR(curl_easy_setopt(curl, CURLOPT_WRITEDATA, &header));
  CHECK_ERROR(curl_easy_perform(curl));
#undef CHECK_ERROR
  return static_cast<long>(header.size_curr - old_size);
}

bool RESTConnection::_check_curl_error(CURLcode x, const std::string& context) const {
  switch (x) {
  case CURLE_OK:
    return true;
  default:
    log_error() << context << ": " << curl_easy_strerror(x) << std::endl;
    return false;
  }
}

#undef CHECK_CURL_ERROR

#else // RESTINSTALLED
int RESTConnection::init() { return -1; }
int RESTConnection::close() { return -1; }
int RESTConnection::nmsg(DIRECTION) const { return -1; }
int RESTConnection::send(utils::Header&) { return -1; }
long RESTConnection::recv(utils::Header&) { return -1; }
#endif // RESTINSTALLED

RESTComm::RESTComm(const std::string name,
		   const utils::Address& address,
		   const DIRECTION direction, int flgs,
		   const COMM_TYPE commtype) :
  CommBase(name, address, direction, commtype, flgs) {
  ADD_CONSTRUCTOR_OPEN(RESTComm)
}

ADD_CONSTRUCTORS_DEF(RESTComm)

void RESTComm::_open(bool call_base) {
  BEFORE_OPEN_DEF;
  updateMaxMsgSize(2048); // Based on limit for GET requests on most servers
  std::string mod;
  if (!partner_model.empty())
    mod = partner_model;
  else if (!model.empty())
    mod = model;
  else
    mod = random_string(4);
  handle = new RESTConnection(logInst(), direction, name,
			      address.address(), mod);
  if (!address.valid())
    address.address(handle->address);
  AFTER_OPEN_DEF;
}

void RESTComm::_close(bool call_base) {
  BEFORE_CLOSE_DEF;
  AFTER_CLOSE_DEF;
}
	
int RESTComm::comm_nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->comm_nmsg(dir);
  if (dir == NONE)
    dir = direction;
  if (dir != direction)
    return 0;
  return handle->nmsg(dir);
}
      
int RESTComm::send_single(utils::Header& header) {
  assert((!global_comm) && handle);
  if (header.on_send() < 0)
    return -1;
  log_debug() << "send_single: " << header.size_msg << " bytes" <<
    std::endl;
  int ret = handle->send(header);
  log_debug() << "send_single: returning " << ret << std::endl;
  return ret;
}

long RESTComm::recv_single(utils::Header& header) {
  assert((!global_comm) && handle);
  log_debug() << "recv_single " << std::endl;
  long ret = handle->recv(header);
  log_debug() << "recv_single: returns " << ret << " bytes" << std::endl;
  return ret;
}

WORKER_METHOD_DEFS(RESTComm)

#undef ASSIGNSTR_RESTBYTES
