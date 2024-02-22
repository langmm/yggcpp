#pragma once

#include "utils/tools.hpp"
#include "utils/multiprocessing.hpp"
#include "communicators/CommBase.hpp"

#define MAX_BUFFERS 100
#define MAX_SHARED_MEM_SIZE 2048

namespace YggInterface {
  namespace communicator {

    /** @brief Structure for storing messages in shared memory */
    typedef struct shmbuf_t {
      char buf[MAX_BUFFERS * MAX_SHARED_MEM_SIZE]; /**< Buffer */
      int size[MAX_BUFFERS];                       /**< Size of data in buffer */
      int count;                                   /**< Number of buffers with data */
      int total;                                   /**< Total number of bytes in buffer */
    } shmbuf_t;

    /**
     * @brief Shared memory based communicator
     */
    class BufferComm : public CommBase<YggInterface::utils::ProcessSharedMemory> {
      COMM_DELETE_COPY(BufferComm);
    public:
      COMM_CONSTRUCTOR_CORE_DEC(BufferComm, BUFFER_COMM, true)

      /** \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
      YGG_API int comm_nmsg(DIRECTION dir=NONE) const override;
      
    protected:
      /** \copydoc Comm_t::send_single */
      YGG_API int send_single(utils::Header& header) override;

      /** \copydoc Comm_t::recv_single */
      YGG_API long recv_single(utils::Header& header) override;

      WORKER_METHOD_DECS(BufferComm);
    private:

      shmbuf_t* memory;            /**< Shared memory */
    };
    
  }
}
