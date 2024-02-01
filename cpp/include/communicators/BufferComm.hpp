#pragma once

#include "utils/tools.hpp"
#include "communicators/CommBase.hpp"
#include "communicators/ProcessMutex.hpp"

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
    class BufferComm : public CommBase<ProcessSharedMemory> {
      BufferComm(const BufferComm&) = delete;
      BufferComm& operator=(const BufferComm&) = delete;
    public:
      /**
       * Constructor
       * @param[in] name The name for the communicator, if empty one will be generated
       * @param[in] address The address for the communicator, if empty one will be generated
       * @param[in] direction Enuerated direction for this instance
       * @param[in] flgs Bitwise flags describing the communicator
       * @param[in] type The enumerated type of communicator to create
       */
      YGG_API explicit BufferComm(const std::string name,
				  const utils::Address &address,
				  const DIRECTION direction = NONE,
				  FLAG_TYPE flgs = 0,
				  const COMM_TYPE type = BUFFER_COMM);
      ADD_CONSTRUCTORS_BASE(BufferComm, BUFFER_COMM, true)

      /** \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
      YGG_API int comm_nmsg(DIRECTION dir=NONE) const override;
      
      using Comm_t::send;
      using Comm_t::recv;
      
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
