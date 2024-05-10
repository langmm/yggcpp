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
       * @param[in] head Header containing message data.
       * @param[in] parent Communicator containing this tine
       * @return Negative values indicate an error
       */
      int send(YggInterface::utils::Header& head, ForkComm& parent);
      /**
       * @brief Receive a message from the next tine
       * @param[out] head Header that message should be shored in
       * @param[in,out] meta Metadata to update for the received message
       * @return Negative values indicate an error
       */
      long recv(YggInterface::utils::Header& head,
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
      
      /** \copydoc YggInterface::communicator::Comm_t::send_single */
      YGG_API int send_single(YggInterface::utils::Header& head) override;
      /** \copydoc YggInterface::communicator::Comm_t::recv_single */
      YGG_API long recv_single(YggInterface::utils::Header& head) override;
      
    protected:

      FORK_TYPE forktype;  /**< Enumerated type of fork */
      size_t ncomm;        /**< Number of tines */
      
    };
    
  }
}
