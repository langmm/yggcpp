#pragma once

#include "communicators/CommBase.hpp"
#include "communicators/DefaultComm.hpp"

#define WRAP_METHOD_NORET(name, argsT, args, err, mods)	\
  YGG_API void name argsT mods
#define WRAP_METHOD(name, argsT, args, err, type, mods)	\
  YGG_API type name argsT mods
#define WRAP_METHOD_OVERRIDE(name, argsT, args, err, type, mods)	\
  WRAP_METHOD(name, argsT, args, err, type, mods override)

#define NO_HANDLE(name)				\
  log_error() << #name << ": No wrapped comm" << std::endl
#define THROW_NO_HANDLE(name)			\
  throw_error(#name ": No wrapped comm")

namespace YggInterface {
  namespace communicator {
    /**
     * @brief Wrapper for a generic communicator.
     */
    class WrapComm : public CommBase<Comm_t> {
      WrapComm(const WrapComm&) = delete;
      WrapComm& operator=(const WrapComm&) = delete;
    public:
      /**
       * Constructor
       * @param[in] name The name for the communicator, if empty one will
       *   be generated
       * @param[in] address The address for the communicator, if empty
       *   one will be generated
       * @param[in] direction Enuerated direction for this instance
       * @param[in] flgs Bitwise flags describing the communicator
       * @param[in] type The communicator type
       * @param[in] wraptype enumerated type of wrapped communicator to
       *   created if different from type
       * @param[in] wrapsupp Supplementary comm parameters for the wrapped
       *   communicator
       * @see utils::Address
       */
      YGG_API explicit WrapComm(const std::string name,
				const utils::Address &address,
				const DIRECTION direction = NONE,
				FLAG_TYPE flgs = 0,
				const COMM_TYPE type = DEFAULT_COMM,
				const COMM_TYPE wraptype = NULL_COMM,
				const SupplementCommArgs& wrapsupp=SupplementCommArgs());
      /**
       * Constructor without an address
       * @param[in] nme The name for the communicator, if empty one will
       *   be generated
       * @param[in] dirn Enuerated direction for this instance
       * @param[in] flgs Bitwise flags describing the communicator
       * @param[in] type The communicator type
       * @param[in] wraptype enumerated type of wrapped communicator to
       *   created if different from type
       * @param[in] wrapsupp Supplementary comm parameters for the wrapped
       *   communicator
       */
      YGG_API explicit WrapComm(const std::string nme,
				const DIRECTION dirn,
				FLAG_TYPE flgs = 0,
				const COMM_TYPE type = DEFAULT_COMM,
				const COMM_TYPE wraptype = NULL_COMM,
				const SupplementCommArgs& wrapsupp=SupplementCommArgs());
      /**
       * Constructor without a name
       * @param[in] addr The address for the communicator, if empty
       *   one will be generated
       * @param[in] dirn Enuerated direction for this instance
       * @param[in] flgs Bitwise flags describing the communicator
       * @param[in] type The communicator type
       * @param[in] wraptype enumerated type of wrapped communicator to
       *   created if different from type
       * @param[in] wrapsupp Supplementary comm parameters for the wrapped
       *   communicator
       * @see utils::Address
       */
      YGG_API explicit WrapComm(const utils::Address &addr,
				const DIRECTION dirn,
				FLAG_TYPE flgs = 0,
				const COMM_TYPE type = DEFAULT_COMM,
				const COMM_TYPE wraptype = NULL_COMM,
				const SupplementCommArgs& wrapsupp=SupplementCommArgs());
      /**
       * @brief Constructor to wrap an existing communicator
       * @param[in] comm Communicator to wrap
       */
      YGG_API explicit WrapComm(Comm_t* comm);
      /** @brief Destructor */
      YGG_API ~WrapComm() override;
      
      /** \copydoc YggInterface::communicator::Comm_t::defaultCommType */
      static COMM_TYPE defaultCommType() { return DEFAULT_COMM; }
      /** \copydoc YggInterface::communicator::Comm_t::isInstalled */
      static bool isInstalled() { return DefaultComm::isInstalled(); }

      /**
       * @brief Get the wrapped communicator
       * @return Wrapped communicator
       */
      Comm_t* getWrapped() { return handle; }

    protected:

      /** @brief Initialize properties from the wrapped communicator */
      YGG_API void fromComm();
      /**
       * @brief Check if the wrapped communicator is initialized
       * @return true if initialized, false otherwise
       */
      YGG_API virtual bool checkWrapped() const;
      /** \copydoc YggInterface::communicator::Comm_t::_open */
      YGG_API void _open(bool call_base);
      /** \copydoc YggInterface::communicator::Comm_t::_close */
      YGG_API void _close(bool call_base);
      
    public:
      WRAP_METHOD(nmsg, (DIRECTION dir=NONE), (dir),
		  out = -1, int, const override);
      WRAP_METHOD(getMetadata, (const DIRECTION dir), (dir),
		  THROW_NO_HANDLE(getMetadata),
		  YggInterface::utils::Metadata&, override);
      WRAP_METHOD_NORET(set_timeout_recv, (int64_t new_timeout),
			(new_timeout),
			THROW_NO_HANDLE(set_timeout_recv),
			override);
      WRAP_METHOD(get_timeout_recv, (), (),
		  out = CommBase::get_timeout_recv(), int64_t,
		  const override);
      WRAP_METHOD(wait_for_recv, (const int64_t& tout), (tout),
		  out = -1, int, override);
      WRAP_METHOD_NORET(close, (), (), , override);
      WRAP_METHOD(is_closed, (), (), out = false, bool,
		  const override);
      WRAP_METHOD(is_open, (), (), out = false, bool,
		  const override);
      // WRAP_METHOD(logClass, (), (), out = "", std::string, const override);
      // WRAP_METHOD(logInst, (), (), out = "", std::string, const override);
      WRAP_METHOD(getWorkers, (), (),
		  THROW_NO_HANDLE(getWorkers),
		  WorkerList&, override);
      WRAP_METHOD(send_raw,
		  (const char *data, const size_t &len),
		  (data, len), out = -1, int, override);
      WRAP_METHOD(recv_raw,
		  (char*& data, const size_t &len),
		  (data, len), out = -1, long, override);
      
      using Comm_t::send;
      using Comm_t::recv;

      // Test methods
      WRAP_METHOD(afterSendRecv, (Comm_t* sComm, Comm_t* rComm),
		  (sComm, rComm), out = false, bool, override);
      WRAP_METHOD(genMetadata, (std::string& meta), (meta),
		  out = false, bool, override);
      
    protected:
      WRAP_METHOD(send_single, (utils::Header& header),
		  (header), out = -1, int, override);
      WRAP_METHOD(recv_single, (utils::Header& header),
		  (header), out = -1, long, override);
      WRAP_METHOD(create_header_send, (utils::Header& header),
		  (header), out = false, bool, override);
      WRAP_METHOD(create_worker,
		  (utils::Address& address,
		   const DIRECTION dir, FLAG_TYPE flgs),
		  (address, dir, flgs),
		  out = nullptr, Comm_t*, override);
      WRAP_METHOD(create_worker_send, (utils::Header& header),
		  (header), out = nullptr, Comm_t*, override);
      WRAP_METHOD(create_worker_recv, (utils::Header& header),
		  (header), out = nullptr, Comm_t*, override);
      
    public:
      COMM_TYPE wraptype; /**< Wrapped communicator type */
      SupplementCommArgs wrapsupp; /**< Supplemental parameters for the wrapped comm */
    };
      
  }
}

#undef WRAP_METHOD_NORET
#undef WRAP_METHOD
