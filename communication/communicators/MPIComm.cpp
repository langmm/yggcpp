#include "MPIComm.hpp"
#include <mpi.h>
#include <boost/algorithm/string.hpp>

using namespace communication::communicator;
using namespace communication::utils;
using namespace communication::datatypes;

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)

mpi_registry_t &mpi_registry_t::Clone() const {
    MPI_Comm ncomm;
    MPI_Comm_dup( (MPI_Comm)the_real_comm, &ncomm);
    auto *clone = new mpi_registry_t(ncomm);
    return *clone;
}

/*MPIComm::MPIComm(const Comm_t *comm) : CommBase(comm, MPI_COMM) {
    if (handle == nullptr) {
        handle = new mpi_registry_t(MPI_COMM_WORLD);
        handle->nproc = 0;
        handle->procs.clear();
        handle->tag = 0;
        handle->nproc = 1;
        std::vector<std::string> adrs;
        //boost::split(adrs, address, boost::is_any_of(","));
        handle->nproc += static_cast<int>(adrs.size());

        size_t ibeg, iend;

        for (const auto &a : adrs) {
            ibeg = a.find("[");
            iend = a.find("]");
            if (ibeg != std::string::npos) {
                handle->procs.push_back(stoi(a.substr(ibeg, iend-ibeg)));
            } else {
                handle->procs.push_back(stoi(a));
            }
        }

    }
}*/

MPIComm::MPIComm(const std::string &name, utils::Address *address, const DIRECTION direction) :
        CommBase(nullptr, direction, MPI_COMM) {

    //if (!(comm->flags & COMM_FLAG_VALID))
    //    return -1;
    if (name.empty()) {
        this->name = "tempinitMPI." + address->address();
    }
    handle = new mpi_registry_t(MPI_COMM_WORLD);
    if (handle == nullptr) {
        ygglog_error("init_mpi_comm: Could not alloc MPI registry.");
        return;
    }
    handle->nproc = 0;
    handle->procs.clear();
    handle->tag = 0;
    handle->nproc = 1;
    std::vector<std::string> adrs;
    boost::split(adrs, address->address(), boost::is_any_of(","));
    handle->nproc += static_cast<int>(adrs.size());

    size_t ibeg, iend;

    for (const auto &a : adrs) {
        ibeg = a.find("[");
        iend = a.find("]");
        if (ibeg != std::string::npos) {
            handle->procs.push_back(stoi(a.substr(ibeg, iend-ibeg)));
        } else {
            handle->procs.push_back(stoi(a));
        }
    }
}

MPIComm::~MPIComm() {
    if (handle != nullptr)
        delete handle;
    handle = nullptr;
}

int MPIComm::mpi_comm_source_id() {
    if (direction == SEND)
        return 0;
    if (handle == nullptr) {
        ygglog_error("mpi_comm_source_id(%s): Queue handle is NULL.", name.c_str());
        return -1;
    }
    //mpi_registry_t* reg = (mpi_registry_t*)(x->handle);
    MPI::Status status;
    int address = MPI::ANY_SOURCE;
    handle->Probe(address, handle->tag, status);
    //if (MPI_Probe(address, handle->tag, handle->comm, &status) != MPI::SUCCESS) {
    //    ygglog_error("mpi_comm_source_id(%s): Error in probe for tag = %d",
    //                 name.c_str(), handle->tag);
    //    return -1;
    //}
    if (status.Get_error()) {
        ygglog_error("mpi_comm_source_id(%s): Error in status for tag = %d: %d",
                     name.c_str(), handle->tag, status.Get_error());
        return -1;
    }

    if (status.Is_cancelled()) {
        ygglog_error("mpi_comm_source_id(%s): Request canceled for tag = %d",
                     name.c_str(), handle->tag);
        return -1;
    }
    if (int src = status.Get_source() > 0) {
        for (size_t i = 0; i < handle->nproc; i++) {
            if (handle->procs[i] == src) {
                return src;
            }
        }
    }
    return 0;
}

int MPIComm::comm_nmsg() const {
    int src = mpi_comm_source_id();
    int nmsg = 0;
    if (src < 0) {
        ygglog_error("mpi_comm_nmsg(%s): Error checking messages.", name.c_str());
        return -1;
    } else if (src > 0) {
        nmsg = 1;
    }
    return nmsg;
}

int MPIComm::send(const char *data, const size_t &len) {
    int ret = -1;
    ygglog_debug("mpi_comm_send(%s): %d bytes", name.c_str(), len);
    if (!check_size(len))
        return -1;
    if (handle == nullptr) {
        ygglog_error("mpi_comm_send(%s): Queue handle is NULL.", name.c_str());
        return -1;
    }
    int len_int = (int)(len);
    int adr = static_cast<int>(handle->procs[handle->tag % handle->nproc]);
    handle->Send(&len_int, 1, MPI_INT, adr, handle->tag);
    //if (MPI_Send(&len_int, 1, MPI_INT, adr, handle->tag, handle)) {
    //    ygglog_error("mpi_comm_send(%s): Error sending message size for tag = %d.",
    //                 name.c_str(), handle->tag);
    //    return -1;
    //}
    handle->Send(data, len_int, MPI_CHAR, adr, handle->tag);
    //if (MPI_Send(data, len_int, MPI_CHAR, address, handle->tag, handle->comm)) {
    //    ygglog_error("mpi_comm_send(%s): Error sending message for tag = %d.",
    //                 name.c_str(), handle->tag);
    //    return -1;
    //}
    ygglog_debug("mpi_comm_send(%s): returning %d", name.c_str(), ret);
    handle->tag++;
    return ret;
}

long MPIComm::recv(char** data, const size_t &len, bool allow_realloc) {
    ygglog_debug("mpi_comm_recv(%s)", name.c_str());
    MPI::Status status;
    int adr = mpi_comm_source_id();
    handle->Probe(adr, handle->tag, status);
    if (status.Get_error()) {
        ygglog_error("mpi_comm_nmsg(%s): Error in probe for tag = %d",
                     name.c_str(), handle->tag);
        return -1;
    }
    int len_recv = 0;
    handle->Recv(&len_recv, 1, MPI_INT, adr, handle->tag, status);
    if (status.Get_error()) {
        ygglog_error("mpi_comm_recv(%s): Error receiving message size for tag = %d.",
                     name.c_str(), handle->tag);
        return -1;
    }
    if (len_recv > len) {
        if (allow_realloc) {
            ygglog_debug("mpi_comm_recv(%s): reallocating buffer from %d to %d bytes.",
                         name.c_str(), len, len_recv);
            (*data) = (char*)realloc(*data, len_recv);
            if (*data == nullptr) {
                ygglog_error("mpi_comm_recv(%s): failed to realloc buffer.", name.c_str());
                return -1;
            }
        } else {
            ygglog_error("mpi_comm_recv(%s): buffer (%d bytes) is not large enough for message (%d bytes)",
                         name.c_str(), len, len_recv);
            return -len_recv;
        }
    }
    handle->Recv(*data, len_recv, MPI_CHAR, adr, handle->tag, status);
    if (status.Get_error()) {
        // TODO: Check status?
        ygglog_error("mpi_comm_recv(%s): Error receiving message for tag = %d.",
                     name.c_str(), handle->tag);
        return -1;
    }
    ygglog_debug("mpi_comm_recv(%s): returns %d bytes", name.c_str(), len_recv);
    handle->tag++;
    return len_recv;
}

// Definitions in the case where MPI libraries not installed
#else /*MPIINSTALLED*/

/*!
  @brief Print error message about MPI library not being installed.
 */
static inline
void mpi_install_error() {
  ygglog_error("Compiler flag 'MPIINSTALLED' not defined so MPI bindings are disabled.");
}

/*!
  @brief Perform deallocation for basic communication.
  @param[in] x comm_t* Pointer to communication to deallocate.
  @returns int 1 if there is an error, 0 otherwise.
*/
static inline
int free_mpi_comm(comm_t *x) {
  // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
  UNUSED(x);
#endif
  mpi_install_error();
  return 1;
}

/*!
  @brief Create a new channel.
  @param[in] comm comm_t * Comm structure initialized with new_comm_base.
  @returns int -1 if the address could not be created.
*/
static inline
int new_mpi_address(comm_t *comm) {
  // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
  UNUSED(comm);
#endif
  mpi_install_error();
  return -1;
}

/*!
  @brief Initialize a sysv_mpi communication.
  @param[in] comm comm_t * Comm structure initialized with init_comm_base.
  @returns int -1 if the comm could not be initialized.
 */
static inline
int init_mpi_comm(comm_t *comm) {
  // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
  UNUSED(comm);
#endif
  mpi_install_error();
  return -1;
}

/*!
  @brief Get number of messages in the comm.
  @param[in] x comm_t Communicator to check.
  @returns int Number of messages. -1 indicates an error.
 */
static inline
int mpi_comm_nmsg(const comm_t *x) {
  // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
  UNUSED(x);
#endif
  mpi_install_error();
  return -1;
}

/*!
  @brief Send a message to the comm.
  Send a message smaller than YGG_MSG_MAX bytes to an output comm. If the
  message is larger, it will not be sent.
  @param[in] x comm_t* structure that comm should be sent to.
  @param[in] data character pointer to message that should be sent.
  @param[in] len size_t length of message to be sent.
  @returns int 0 if send succesfull, -1 if send unsuccessful.
 */
static inline
int mpi_comm_send(const comm_t *x, const char *data, const size_t len) {
  // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
  UNUSED(x);
  UNUSED(data);
  UNUSED(len);
#endif
  mpi_install_error();
  return -1;
}

/*!
  @brief Receive a message from an input comm.
  Receive a message smaller than YGG_MSG_MAX bytes from an input comm.
  @param[in] x comm_t* structure that message should be sent to.
  @param[out] data char ** pointer to allocated buffer where the message
  should be saved. This should be a malloc'd buffer if allow_realloc is 1.
  @param[in] len const size_t length of the allocated message buffer in bytes.
  @param[in] allow_realloc const int If 1, the buffer will be realloced if it
  is not large enought. Otherwise an error will be returned.
  @returns int -1 if message could not be received. Length of the received
  message if message was received.
 */
static inline
int mpi_comm_recv(const comm_t *x, char **data, const size_t len,
		  const int allow_realloc) {
  // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
  UNUSED(x);
  UNUSED(data);
  UNUSED(len);
  UNUSED(allow_realloc);
#endif
  mpi_install_error();
  return -1;
}


#endif