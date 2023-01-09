#pragma once

#ifdef _YGGIPC
#include "utils/tools.hpp"


#include <fcntl.h>           /* For O_* constants */
#include <sys/stat.h>        /* For mode constants */
#include <sys/msg.h>
#include <sys/types.h>
#include <sys/sem.h>
#include <sys/shm.h>

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
    char data[YGG_MSG_MAX]; //!< Buffer for the message
} msgbuf_t;
//class ClientComm;
//class ServerComm;
class IPCComm : public CommBase<int, int> {
public:
    explicit IPCComm(const std::string &name = "", utils::Address *address = new utils::Address(),
                     DIRECTION direction = NONE);

    //explicit IPCComm(Comm_t* comm);
    ~IPCComm() override;

    int check_channels();

    void add_channel();

    int remove_comm(bool close_comm);

    int comm_nmsg() const override;

    int send(const char *data, const size_t &len) override;

    long recv(char *data, const size_t &len, bool allow_realloc) override;

protected:
    virtual int new_address();
    void init() override;

private:
    friend class ClientComm;
    friend class ServerComm;
    /*! @brief Names of channels in use. */
    static int _yggChannelNames[_yggTrackChannels];
    /*! @brief Number of channels in use. */
    static unsigned _yggChannelsUsed;
    static bool _ipc_rand_seeded;

    int send_normal(const char *data, const size_t &len);

    int send_large(const char *data, const size_t &len);
};

}
}

#endif
