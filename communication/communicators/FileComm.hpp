#pragma once

#include "utils/tools.hpp"

#include <fstream>
#include <cstdio>
#ifdef _MSC_VER
#include <io.h>
#endif // _MSC_VER

#include "CommBase.hpp"

namespace communication {
  namespace communicator {

    class YGG_API FileComm : public CommBase<std::fstream> {
    public:
      /**
       * Constructor for a file based communicator
       * @param name The name for the communicator, if not given one will be generated
       * @param address The address for the communicator, if not given one will be generated
       * @param direction Enuerated direction for this instance
       * @param flags Bitwise flags describing the communicator
       */
      explicit FileComm(const std::string name = "",
			utils::Address *address = new utils::Address(),
			const DIRECTION direction = NONE,
			int flgs = 0, const COMM_TYPE type = FILE_COMM);
      ADD_CONSTRUCTORS_BASE(FileComm, FILE_COMM, true)

      /*! \copydoc Comm_t::close */
      void close() override;
      
      /**
       * The number of messages in the queue
       * @return The number of messages
       */
      int comm_nmsg(DIRECTION dir=NONE) const override;
      using Comm_t::send;
      using Comm_t::recv;
      
    protected:
      void init();
      void refresh() const;
      
      /*! \copydoc Comm_t::send_single */
      int send_single(utils::Header& header) override;

      /*! \copydoc Comm_t::recv_single */
      long recv_single(utils::Header& header) override;

      WORKER_METHOD_DECS(FileComm);

    private:
      std::fstream::openmode mode;
      
    };

  }
}
