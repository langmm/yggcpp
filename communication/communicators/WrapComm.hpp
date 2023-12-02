#pragma once

#include "CommBase.hpp"
#include "DefaultComm.hpp"

#define WRAP_METHOD_NORET(name, argsT, args, err, mods)	\
  void name argsT mods
#define WRAP_METHOD(name, argsT, args, err, type, mods)	\
  type name argsT mods
#define WRAP_METHOD_OVERRIDE(name, argsT, args, err, type, mods)	\
  WRAP_METHOD(name, argsT, args, err, type, mods override)

#define NO_HANDLE(name)				\
  log_error() << #name << ": No wrapped comm" << std::endl
#define THROW_NO_HANDLE(name)			\
  throw_error(#name ": No wrapped comm")

namespace communication {
  namespace communicator {
    class YGG_API WrapComm : public CommBase<Comm_t> {
    public:
      explicit WrapComm(const std::string name,
			const utils::Address &address,
			const DIRECTION direction = NONE, int flgs = 0,
			const COMM_TYPE type = DEFAULT_COMM,
			const COMM_TYPE wraptype = NULL_COMM,
			bool delay_init=false);
      explicit WrapComm(const std::string nme,
			const DIRECTION dirn, int flgs = 0,
			const COMM_TYPE type = DEFAULT_COMM,
			const COMM_TYPE wraptype = NULL_COMM);
      explicit WrapComm(const utils::Address &addr,
			const DIRECTION dirn, int flgs = 0,
			const COMM_TYPE type = DEFAULT_COMM,
			const COMM_TYPE wraptype = NULL_COMM);
      explicit WrapComm(Comm_t* comm);
      
      // \copydoc Comm_t::defaultCommType
      static COMM_TYPE defaultCommType() { return DEFAULT_COMM; }
      // \copydoc Comm_t::isInstalled
      static bool isInstalled() { return DefaultComm::isInstalled(); }

      Comm_t* getWrapped() { return handle; }

    protected:
      void init();
      void fromComm();
      virtual bool checkWrapped() const;
      void _close(bool call_base);
      
    public:
      WRAP_METHOD(comm_nmsg, (DIRECTION dir=NONE), (dir),
		  out = -1, int, const override);
      WRAP_METHOD(getMetadata, (const DIRECTION dir), (dir),
		  THROW_NO_HANDLE(getMetadata),
		  communication::utils::Metadata&, override);
      WRAP_METHOD_NORET(set_timeout_recv, (int64_t new_timeout),
			(new_timeout),
			THROW_NO_HANDLE(set_timeout_recv), override);
      WRAP_METHOD(get_timeout_recv, (), (),
		  out = CommBase::get_timeout_recv(), int64_t, override);
      WRAP_METHOD(wait_for_recv, (const int64_t& tout), (tout),
		  out = -1, int, override);
      WRAP_METHOD_NORET(close, (), (), , override);
      WRAP_METHOD(is_closed, (), (), out = false, bool, const override);
      WRAP_METHOD(is_open, (), (), out = false, bool, const override);
      // WRAP_METHOD(logClass, (), (), out = "", std::string, const override);
      // WRAP_METHOD(logInst, (), (), out = "", std::string, const override);
      WRAP_METHOD(getWorkers, (), (), THROW_NO_HANDLE(getWorkers), WorkerList&, override);
      
      using Comm_t::send;
      using Comm_t::recv;

#ifdef YGG_TEST
      WRAP_METHOD(afterSendRecv, (Comm_t* sComm, Comm_t* rComm),
		  (sComm, rComm), out = false, bool, override);
#endif
      
    protected:
      WRAP_METHOD(send_single, (utils::Header& header),
		  (header), out = -1, int, override);
      WRAP_METHOD(recv_single, (utils::Header& header),
		  (header), out = -1, long, override);
      WRAP_METHOD(create_header_send, (utils::Header& header),
		  (header), out = false, bool, override);
      WRAP_METHOD(create_worker,
		  (utils::Address& address,
		   const DIRECTION& dir, int flgs),
		  (address, dir, flgs),
		  out = nullptr, Comm_t*, override);
      WRAP_METHOD(create_worker_send, (utils::Header& header),
		  (header), out = nullptr, Comm_t*, override);
      WRAP_METHOD(create_worker_recv, (utils::Header& header),
		  (header), out = nullptr, Comm_t*, override);
      COMM_TYPE wraptype;
    };
      
  }
}

#undef WRAP_METHOD_NORET
#undef WRAP_METHOD
