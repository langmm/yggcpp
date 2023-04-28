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

static std::string blank = "";

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
    /**
     * Constructor for an IPC based communicator
     * @param name The name for the communicator, if not given one will be generated
     * @param address The address for the communicator, if not given one will be generated
     * @param direction Enuerated direction for this instance
     */
    explicit IPCComm(const std::string &name = blank, utils::Address *address = new utils::Address(),
                     DIRECTION direction = NONE);

    //explicit IPCComm(Comm_t* comm);
    /**
     * Destructor
     */
    ~IPCComm() override;

    /**
     * Check if an IPC channel can be created
     * @return
     */
    int check_channels();

    /**
     * Add a new channel to the list of channels
     * @return
     */
    void add_channel();

    /**
     * Remove the given ipc queue
     * @param close_comm If 1, close the queue, otherwise remove the given comm from the register
     * @return -1 on error
     */
    int remove_comm(bool close_comm);

    /**
     * The number of messages in the queue
     * @return The number of messages
     */
    int comm_nmsg() const override;
    using Comm_t::send;
    using Comm_t::recv;

#ifndef YGG_TEST
protected:
#endif
    /**
     * Generate a new address
     * @return Success or failure
     */
    virtual bool new_address();

    /**
     * Initialize the class
     */
    void init() override;

    /**
     * Sending function
     * @param data The message to send
     * @param len The length of data
     * @return THe status
     */
    int send(const char *data, const size_t &len) override;

    /**
     * Receiving function
     * @param data The contents of the message withh be placed here
     * @param len The initial length of data
     * @param allow_realloc Whether data can be reallocated if it is too small to hold the message.
     * @return The length of data after the message was copied.
     */
    long recv(char *data, const size_t &len, bool allow_realloc) override;

private:
    friend class ClientComm;
    friend class ServerComm;
    /*! @brief Names of channels in use. */
    static int _yggChannelNames[_yggTrackChannels];
    /*! @brief Number of channels in use. */
    static unsigned _yggChannelsUsed;
    static bool _ipc_rand_seeded;

    /**
     * Send a standard size message
     * @param data The message to send
     * @param len The length of data
     * @return The status
     */
    int send_normal(const char *data, const size_t &len);

    /**
     * Send a large message, by breaking it up into smaller messages
     * @param data The message to send
     * @param len The length of data
     * @return The status
     */
    int send_large(const char *data, const size_t &len);
};

}
}

#endif
