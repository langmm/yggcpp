#pragma once

#include "utils/tools.hpp"

#ifdef IPCINSTALLED
#include <fcntl.h>           /* For O_* constants */
#include <sys/stat.h>        /* For mode constants */
#include <sys/msg.h>
#include <sys/types.h>
#include <sys/sem.h>
#include <sys/shm.h>
#endif // IPCINSTALLED

#include "CommBase.hpp"

/*! @brief Maximum number of channels. */
#define _yggTrackChannels 256

namespace communication {
namespace communicator {
/*!
  @brief Message buffer structure.
*/
typedef struct msgbuf_t {
    long mtype; //!< Message buffer type
    char data[2048]; //!< Buffer for the message
} msgbuf_t;
  
//class ClientComm;
//class ServerComm;
class IPCComm : public CommBase<int> {
public:
    explicit IPCComm(const std::string name = "",
		     utils::Address *address = new utils::Address(),
                     DIRECTION direction = NONE, int flgs = 0);
    ADD_CONSTRUCTORS(IPCComm)

    //explicit IPCComm(Comm_t* comm);
    ~IPCComm() override;

    int check_key(int key);

    void add_channel();

    int remove_comm(bool close_comm);

    int comm_nmsg() const override;
    using Comm_t::send;
    using Comm_t::recv;

#ifndef YGG_TEST
protected:
#endif
    void init();
    int send_single(const char *data, const size_t &len,
		    const Header& header) override;

    long recv_single(char*& data, const size_t &len, bool allow_realloc) override;
    WORKER_METHOD_DECS(IPCComm);

private:
    friend class ClientComm;
    friend class ServerComm;
    /*! @brief Names of channels in use. */
    static int _yggChannelNames[_yggTrackChannels];
    /*! @brief Number of channels in use. */
    static unsigned _yggChannelsUsed;
    static bool _ipc_rand_seeded;

};

}
}
