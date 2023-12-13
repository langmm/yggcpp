#pragma once

#include "CommBase.hpp"
#include "utils/serialization.hpp"
#include <atomic>

namespace YggInterface {
namespace communicator {
    /**
     *  @brief Asynchronous backlog class for holding data to be transmitted
     */
    class AsyncBacklog {
    public:
        AsyncBacklog(const AsyncBacklog&) = delete;
        AsyncBacklog& operator=(const AsyncBacklog&) = delete;
        /**
         * @brief Create a new instance
         * @param[in] parent The parent communicator
         */
        explicit AsyncBacklog(Comm_t* parent);
        ~AsyncBacklog();
        /**
         * @brief Function to run in a thread which sends/receives messages
         * @param[in] parent The parent communicator
         * @return Always returns false, when the thread is shut down.
         */
        bool on_thread(Comm_t* parent);
        /**
         * @brief Send a message from the backlog
         * @return The length of data sent, in bytes.
         */
        int send();
        /**
         * @brief Receive message into the backlog
         * @return The length of data received, in bytes.
         */
        long recv();
        Comm_t* comm;             /**< parent communicator */
        std::mutex comm_mutex;    /**< communicator lock */
        std::atomic_bool opened;  /**< whether the communicator is open */
        std::atomic_bool closing; /**< whether the communicator is shutting down */
        std::atomic_bool locked;  /**< whether the communicator is locked */
        std::vector<utils::Header> backlog;  /**< messages to be processed */
        std::thread backlog_thread;  /**< thread for sending/receiving messages */
    };

    /**
     * @brief Lock guard/mutex for asynchronous communication with a AsyncBacklog class.
     */
    class AsyncLockGuard {
    public:
        AsyncLockGuard(const AsyncLockGuard&) = delete;
        AsyncLockGuard& operator=(const AsyncLockGuard&) = delete;
        /**
         * @brief Create an instance
         * @param[in] backlog Instance of the AsyncBacklog class being used for communication
         * @param[in] dont_lock If true, then immediately lock the mutex.
         */
        explicit AsyncLockGuard(AsyncBacklog* backlog, bool dont_lock=false);
        ~AsyncLockGuard();
        bool locked;     /**< indicates whether the lock is currently enabled */
        AsyncBacklog* backlog;  /**< the AsyncBacklog instance to work with */
    };

    /**
     * @brief Asynchonous communication class.
     **/
    class AsyncComm : public CommBase<AsyncBacklog> {
    public:
        /**
         * @brief Constructor
         * @param[in] name The name of the communicator
         * @param[in] address The address to associate with the communicator,
         *   if the address is nullptr, then an address will be created.
         * @param[in] direction Enumerated direction for communicator
         * @param[in] flgs Bitwise flags describing the communicator
         * @param[in] type Enumerated communicator type
         * @see utils::Address
         **/
        explicit AsyncComm(const std::string& name,
                           utils::Address& address,
                           const DIRECTION direction = NONE, int flgs = 0,
                           const COMM_TYPE type = DEFAULT_COMM);
        /**
         * @brief Constructor
         * @param[in] name The name of the communicator
         * @param[in] direction Enumerated direction for communicator
         * @param[in] flgs Bitwise flags describing the communicator
         * @param[in] type Enumerated communicator type
         **/
        explicit AsyncComm(const std::string& name,
                           const DIRECTION direction, int flgs = 0,
                           const COMM_TYPE type = DEFAULT_COMM);
        /**
         * @brief Constructor
         * @param[in] address The address to associate with the communicator,
         *   if the address is nullptr, then an address will be created.
         * @param[in] direction Enumerated direction for communicator
         * @param[in] flgs Bitwise flags describing the communicator
         * @param[in] type Enumerated communicator type
         * @see utils::Address
         **/
        explicit AsyncComm(utils::Address &address,
                           const DIRECTION direction, int flgs = 0,
                           const COMM_TYPE type = DEFAULT_COMM);

        /**
         * Get the default communications type
         * @return Enumeration of the default communications type
         */
        static COMM_TYPE defaultCommType() { return DEFAULT_COMM; }

        /** \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
        int comm_nmsg(DIRECTION dir=NONE) const override;
        /** \copydoc YggInterface::communicator::Comm_t::getMetadata */
        YggInterface::utils::Metadata& getMetadata(DIRECTION dir) override;
        /** \copydoc YggInterface::communicator::Comm_t::set_timeout_recv */
        void set_timeout_recv(int new_timeout) override;
        /** \copydoc YggInterface::communicator::Comm_t::get_timeout_recv */
        int get_timeout_recv() override;

      using Comm_t::send;
      using Comm_t::recv;

    protected:
        /** \copydoc YggInterface::communicator::Comm_t::send_single */
        int send_single(utils::Header& header) override;
        /** \copydoc YggInterface::communicator::Comm_t::recv_single */
        long recv_single(utils::Header& header) override;
        /** \copydoc YggInterface::communicator::Comm_t::create_header_send */
        bool create_header_send(utils::Header& header) override;
        /** \copydoc YggInterface::communicator::Comm_t::create_worker */
        Comm_t* create_worker(utils::Address& address,
                              const DIRECTION dir, int flgs) override;

    };

  }
} // YggInterface
