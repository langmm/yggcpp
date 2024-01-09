#pragma once

#include "utils/tools.hpp"

#include <fstream>
#include <cstdio>
#ifdef _MSC_VER
#include <io.h>
#endif // _MSC_VER

#include "CommBase.hpp"

namespace YggInterface {
  namespace communicator {

    /**
     * @brief File based communicator
     */
    class YGG_API FileComm : public CommBase<std::fstream> {
    public:
      /**
       * Constructor
       * @param[in] name The name for the communicator, if empty one will be generated
       * @param[in] address The address for the communicator, if empty one will be generated
       * @param[in] direction Enuerated direction for this instance
       * @param[in] flgs Bitwise flags describing the communicator
       * @param[in] type The enumerated type of communicator to create
       */
      explicit FileComm(const std::string name,
			const utils::Address &address,
			const DIRECTION direction = NONE,
			int flgs = 0, const COMM_TYPE type = FILE_COMM);
      ADD_CONSTRUCTORS_BASE(FileComm, FILE_COMM, true)

      /** \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
      int comm_nmsg(DIRECTION dir=NONE) const override;
      using Comm_t::send;
      using Comm_t::recv;
      
    protected:
      /*! \copydoc Comm_t::init */
      void init();
      /*! @brief Flush and reload the file */
      void refresh() const;
      
      /*! \copydoc Comm_t::send_single */
      int send_single(utils::Header& header) override;

      /*! \copydoc Comm_t::recv_single */
      long recv_single(utils::Header& header) override;

      WORKER_METHOD_DECS(FileComm);

    private:
      std::fstream::openmode mode; //!< Mode used to open the file
      
    };

  }
}
