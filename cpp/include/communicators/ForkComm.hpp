#pragma once

#include "communicators/CommBase.hpp"

namespace YggInterface {
  namespace communicator {

    class ForkComm;

    /**
     * @brief Forked communication engine for coordinating send/recv
     *   actions for the set of 'tine' communicator.
     */
    class ForkTines : public YggInterface::utils::LogBase {
    private:
      ForkTines(const ForkTines&) = delete;
      ForkTines& operator=(const ForkTines&) = delete;
    public:
      /**
       * @brief Constructor using an existing set of tine communicators
       * @param[in] logInst String that should be used in log messages
       *   identifying the instance
       * @param[in] comm Tine communicators
       * @param[in] typ Enumerated fork type
       */
      ForkTines(const std::string logInst,
		std::vector<Comm_t*>& comm,
		const FORK_TYPE typ=FORK_DEFAULT);
      /**
       * @brief Constructor to create a new set of tine communicators
       * @param[in] logInst String that should be used in log messages
       *   identifying the instance
       * @param[in] names Names for tine communicators
       * @param[in] addresses Addresses for tine communicators
       * @param[in] dir Direction of tine communicators
       * @param[in] flags Flags for tine communicators
       * @param[in] commtype Communicator type for tine communicators
       * @param[in] forktype Enumerated fork type
       */
      ForkTines(const std::string logInst,
		const std::vector<std::string>& names,
		const std::vector<std::string>& addresses,
		const DIRECTION dir, FLAG_TYPE flags=0,
		const COMM_TYPE commtype=DEFAULT_COMM,
		const FORK_TYPE forktype=FORK_DEFAULT);
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "ForkTines"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return logInst_; }
      /** @brief Destructor */
      ~ForkTines();
      /** @brief Update the fork type based on other properties */
      void updateType();
      /** @brief Close the fork tine communicators */
      void close();
      /**
       * @brief Send an EOF message to tines
       * @return true if successful, false otherwise
       */
      bool eof() const;
      /**
       * @brief Get the tine communicator that should be sent to or
       *   received from next
       * @return Tine communicator
       */
      Comm_t* current_cycle();
      /** \copydoc YggInterface::communicator::Comm_t::nmsg */
      int nmsg(DIRECTION dir) const;
      /**
       * @brief Send a message to the next tine
       * @param[in] data Message to send
       * @param[in] len Size of data (bytes)
       * @param[in] parent Communicator containing this tine
       * @return Negative values indicate an error
       */
      int send(const char *data, const size_t &len,
	       ForkComm& parent);
      /**
       * @brief Receive a message from the next tine
       * @param[out] data Buffer to store the received message in
       * @param[in] len Size of data
       * @param[in,out] meta Metadata to update for the received message
       * @return Negative values indicate an error
       */
      long recv(char*& data, const size_t &len,
		YggInterface::utils::Metadata& meta);
      std::string logInst_;       /**< Identifier to use for instance in log messages */
      FORK_TYPE forktype;         /**< Enumerated type of fork */
      std::vector<Comm_t*> comms; /**< Tine communicators */
      FLAG_TYPE iter;              /**< Current iteration within tine cycle */
    };

    /**
     * @brief Forked communicator class for sending/receiving to/from
     *    multiple communicators (tines).
     */
    class ForkComm : public CommBase<ForkTines> {
    public:
      COMM_CONSTRUCTOR_CORE_DEC(ForkComm, DEFAULT_COMM, true)

      /** \copydoc YggInterface::communicator::Comm_t::nmsg */
      YGG_API int nmsg(DIRECTION dir=NONE) const override;
      /** \copydoc YggInterface::communicator::Comm_t::set_timeout_recv */
      YGG_API void set_timeout_recv(int64_t new_timeout) override;
      
      /** \copydoc YggInterface::communicator::Comm_t::send_raw */
      YGG_API int send_raw(const char *data, const size_t &len) override;
      /** \copydoc YggInterface::communicator::Comm_t::recv_raw */
      YGG_API long recv_raw(char*& data, const size_t &len) override;
      
    protected:

      FORK_TYPE forktype;  /**< Enumerated type of fork */
      size_t ncomm;        /**< Number of tines */
      
    };
    
  }
}
