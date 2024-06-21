#pragma once

// TODO:
// - Methods for creating a wrapper for C/C++ methods
// - Type conversion
// - Store type associated with function and use it to set the schema?
// - Allow for multiple copies in the same model?
// - Dynamic library lookup: Create dynamic library with wrapped, compiled
//   model functions, pass the name in an environment variable
//   (YGG_FUNCTION_LIBRARY), check that the file exists, dynamically load
//   it, search for functions, register function

#include "utils/tools.hpp"
#include "communicators/CommBase.hpp"
#include "datatypes/dtype_t.h"

#include <functional>

namespace YggInterface {
  namespace communicator {

    /** @brief C++ function type using C++ types */
    typedef std::function<bool(const rapidjson::Document&, rapidjson::Document&)> cxx_function;
    /** @brief C++ function type using C description */
    typedef bool cxx_function_alt (const rapidjson::Document&, rapidjson::Document&);
    
    /**
     * @brief Function wrapper.
     */
    class FunctionWrapper : public YggInterface::utils::LogBase {
      FunctionWrapper(const FunctionWrapper&) = delete;
      FunctionWrapper& operator=(const FunctionWrapper&) = delete;
    public:
      
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "FunctionWrapper"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return address; }
      /**
       * Constructor for wrapping a model imported based on a string.
       * @param[in] f Function name and import information.
       * @param[in] pointer_provided If true, the constructor is being
       *   called with an explicit function pointer.
       */
      FunctionWrapper(const std::string& f, bool pointer_provided=false);
      /**
       * Constructor for wrapping a C++ function
       * @param[in] name Name of the function
       * @param[in] f Function
       */
      FunctionWrapper(const std::string& name, cxx_function& f);
      /**
       * Constructor for wrapping a C function
       * @param[in] name Name of the function
       * @param[in] f Function
       */
      FunctionWrapper(const std::string& name, c_function& f);
      /**
       * Destructor
       */
      ~FunctionWrapper();

      /**
       * @brief Send a message to the function.
       * @param[in] data Document containing function arguments.
       * @returns true on success, false otherwise.
       */
      bool send(const rapidjson::Document& data);
      /**
       * @brief Receive a message from the function.
       * @param[in,out] data Document to receive message into.
       * @returns true on success, false otherwise.
       */
      bool recv(rapidjson::Document& data);
      /**
       * @brief Get the number of messages in the receive backlog.
       * @return Number of messages in backlog.
       */
      int nmsg() const;
      /**
       * @brief Clear any message in the receive backlog.
       */
      void clear();
      
      std::string address; /**< Name of the function */
      LANGUAGE language;   /**< Language the function is written in */
      
    private:
      void* func;          /**< Pointer to the function */
      std::vector<rapidjson::Document> recv_backlog; /**< Backlog of function call results */
      
      /**
       * Call the wrapped method.
       * @param[in] data_send Message containing function arguments.
       * @param[in,out] data_recv Document where function results should
       *   be stored.
       * @returns true on success, false otherwise.
       */
      bool _call(const rapidjson::Document& data_send,
		 rapidjson::Document& data_recv);
    };

    /**
     * @brief Function function call based communicator
     */
    class FunctionComm : public CommBase<FunctionWrapper> {
    public:
      COMM_CONSTRUCTOR_CORE_DEC(FunctionComm, FUNCTION_COMM, true)

      /** \copydoc YggInterface::communicator::Comm_t::nmsg */
      YGG_API int nmsg(DIRECTION dir=NONE) const override;
      
    protected:
      /** \copydoc YggInterface::communicator::Comm_t::send_single */
      YGG_API int send_single(utils::Header& header) override;

      /** \copydoc YggInterface::communicator::Comm_t::recv_single */
      YGG_API long recv_single(utils::Header& header) override;
      
    };

    /**
     * @brief Register a C++ function
     * @param[in] name Function name with language prefix
     * @param[in] func Function to register
     * @param[in] no_prefix If true, the "cxx::" prefix will not be
     *   added to the name (usually because another language has
     *   already been added).
     */
    YGG_API void register_function(const std::string& name,
				   cxx_function& func,
				   bool no_prefix=false);
    /**
     * @brief Register a C++ function
     * @param[in] name Function name with language prefix
     * @param[in] func Function to register
     * @param[in] no_prefix If true, the "cxx::" prefix will not be
     *   added to the name (usually because another language has
     *   already been added).
     */
    YGG_API void register_function(const std::string& name,
				   cxx_function_alt& func,
				   bool no_prefix=false);
    /**
     * @brief Register a C function
     * @param[in] name Function name with language prefix
     * @param[in] func Function to register
     * @param[in] no_prefix If true, the "c::" prefix will not be
     *   added to the name (usually because another language has
     *   already been added).
     */
    YGG_API void register_function(const std::string& name,
				   c_function& func,
				   bool no_prefix=false);
      
  }
}
