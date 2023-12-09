#pragma once

#include "CommBase.hpp"

namespace communication {
  namespace communicator {

    class ForkTines : public communication::utils::LogBase {
    private:
      ForkTines(const ForkTines&) = delete;
      ForkTines& operator=(const ForkTines&) = delete;
    public:
      ForkTines(const std::string logInst,
		std::vector<Comm_t*>& comm,
		const FORK_TYPE typ=FORK_DEFAULT);
      ForkTines(const std::string logInst,
		const std::vector<std::string>& names,
		const std::vector<std::string>& addresses,
		const DIRECTION dir, int flags=0,
		const COMM_TYPE commtype=DEFAULT_COMM,
		const FORK_TYPE forktype=FORK_DEFAULT);
      std::string logClass() const override { return "ForkTines"; }
      std::string logInst() const override { return logInst_; }
      ~ForkTines();
      void updateType();
      void close();
      bool eof() const;
      Comm_t* current_cycle();
      int nmsg(DIRECTION dir) const;
      int send(const char *data, const size_t &len,
	       communication::utils::Metadata& meta);
      long recv(char*& data, const size_t &len,
		communication::utils::Metadata& meta);
      std::string logInst_;
      FORK_TYPE forktype;
      std::vector<Comm_t*> comms;
      uint64_t iter;
    };

    class YGG_API ForkComm : public CommBase<ForkTines> {
    public:

      explicit ForkComm(const std::string name,
			const utils::Address& address,
			const DIRECTION direction=NONE, int flgs=0,
			const COMM_TYPE commtype=DEFAULT_COMM,
			size_t ncomm=0);
      ForkComm(const std::string nme, const DIRECTION dirn,
	       int flgs=0, const COMM_TYPE commtype=DEFAULT_COMM,
	       size_t ncomm=0);
      explicit ForkComm(utils::Address &addr, const DIRECTION dirn,
			int flgs=0, const COMM_TYPE type=DEFAULT_COMM,
			size_t ncomm=0);
      ADD_METHODS_BASE(ForkComm, DEFAULT_COMM, true)
	
      // \copydoc Comm_t::comm_nmsg
      int comm_nmsg(DIRECTION dir=NONE) const override;
      // \copydoc Comm_t::set_timeout_recv
      void set_timeout_recv(int64_t new_timeout) override;
      
      using Comm_t::send;
      using Comm_t::recv;

    protected:
      void init();
      int send_raw(const char *data, const size_t &len) override;
      long recv_raw(char*& data, const size_t &len,
		    bool allow_realloc=false) override;

      FORK_TYPE forktype;
      size_t ncomm;
      
    };
    
  }
}
