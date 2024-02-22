#pragma once

#include "utils/tools.hpp"
#include "utils/multiprocessing.hpp"

#include <fstream>
#include <cstdio>
#ifdef _MSC_VER
#include <io.h>
#endif // _MSC_VER

#include "communicators/CommBase.hpp"

namespace YggInterface {
  namespace communicator {

    /**
     * @brief File based communicator
     */
    class FileComm : public CommBase<std::fstream> {
    public:
      COMM_CONSTRUCTOR_CORE_DEC(FileComm, FILE_COMM, true)

      /** \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
      YGG_API int comm_nmsg(DIRECTION dir=NONE) const override;
      
    protected:
      /*! @brief Flush and reload the file */
      void refresh() const;
      
      /*! \copydoc Comm_t::send_single */
      YGG_API int send_single(utils::Header& header) override;

      /*! \copydoc Comm_t::recv_single */
      YGG_API long recv_single(utils::Header& header) override;

      WORKER_METHOD_DECS(FileComm);

    private:
      std::fstream::openmode mode; //!< Mode used to open the file
      YggInterface::utils::ProcessMutex mutex; //!< Mutex for ensuring that a file is not accessed by two processes
      
    };

  }
}
