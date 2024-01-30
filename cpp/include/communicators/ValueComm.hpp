#pragma once

#include "communicators/CommBase.hpp"

namespace YggInterface {
  namespace communicator {
    /**
     * @brief Value manager for ValueComm
     */
    class ValueManager : public YggInterface::utils::LogBase {
      ValueManager(const ValueManager&) = delete;
      ValueManager& operator=(const ValueManager&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] logInst String that should be used in log messages
       *   identifying the instance
       */
      ValueManager(const std::string logInst);
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "ValueManager"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return logInst_; }
      /** @brief Destructor */
      ~ValueManager();
      /**
       * @brief Update the value and number of values returned on receive
       * @tparam T Type of value
       * @param[in] val Value that should be returned on receive
       * @param[in] N Number of times value should be returned
       */
      template<typename T>
      void update(const T& val, int N) {
	rapidjson::Document d;
	d.Set(val, d.GetAllocator());
	rapidjson::StringBuffer buffer;
	rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
	d.Accept(writer);
	count = N;
	value.assign(static_cast<const char*>(buffer.GetString()),
		     static_cast<size_t>(buffer.GetLength()));
      }

      /**
       * @brief Return the number of remaining values
       * @return Number of remaining values, -1 if there is an error.
       */
      int remaining() const;

      /**
       * @brief Return a value and increment the count
       * @param[out] header Header that value should be stored in
       * @return Result of receive. Negative values indicate errors
       */
      long recv(utils::Header& header);

      std::string logInst_; /**< Identifier to use for instance in log messages */
      std::string value;    /**< Serialized value to return on recieve */
      int count;            /**< Number of times to return value */
      int index;            /**< Number of times value has been returned */
    };

    /**
     * @brief Value based communicator
     */
    class ValueComm : public CommBase<ValueManager> {
    public:
      
      /**
       * Constructor
       * @param[in] name The name for the communicator, if empty one will be generated
       * @param[in] address The address for the communicator, if empty one will be generated
       * @param[in] direction Enuerated direction for this instance
       * @param[in] flgs Bitwise flags describing the communicator
       * @param[in] commtype The enumerated type of communicator to create
       */
      YGG_API explicit ValueComm(const std::string name,
				 const utils::Address& address,
				 const DIRECTION direction=NONE,
				 FLAG_TYPE flgs=0,
				 const COMM_TYPE commtype=VALUE_COMM);
      ADD_CONSTRUCTORS_BASE(ValueComm, VALUE_COMM, true)
	
      /** \copydoc Comm_t::comm_nmsg */
      YGG_API int comm_nmsg(DIRECTION dir=NONE) const override;
      
      using Comm_t::send;
      using Comm_t::recv;
      
      /**
       * @brief Update the value and number of values returned on receive
       * @tparam T Type of value
       * @param[in] value Value that should be returned on receive
       * @param[in] count Number of times value should be returned
       * @return true if successful, false otherwise
       */
      template<typename T>
      bool setValue(const T& value, int count=-1) {
	assert(handle);
	handle->update(value, count);
	return true;
      }

    protected:
      /** \copydoc Comm_t::send_single */
      int send_single(utils::Header& header) override;

      /** \copydoc Comm_t::recv_single */
      long recv_single(utils::Header& header) override;

      WORKER_METHOD_DECS(ValueComm);
      
    };
  }
}
