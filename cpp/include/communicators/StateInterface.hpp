#pragma once
#include <string>
#include <functional>
#include "utils/logging.hpp"
#include "communicators/ServerComm.hpp"
#include "datatypes/dtype_t.h"


using namespace yggdrasil_rapidjson;

namespace YggInterface {

namespace communicator {


/**
   @brief Virtual base class for implementing the function signature
     expected by StateInterface.
 */
class StateFunction {
public:
  /** @brief Constructor */
  YGG_API StateFunction();
  /** @brief Destructor */
  YGG_API virtual ~StateFunction();
  /**
   * @brief Call the function.
   * @param[in] name Name of state variable/action.
   * @param[in,out] data Value for set/get/action.
   * @return true if successful, false otherwise
   */
  YGG_API virtual bool operator()(const std::string& name,
                                  yggdrasil_rapidjson::Document& data);
  /**
   * @brief Return a copy of this function
   * @return Copy
   */
  YGG_API virtual StateFunction* copy() const;
};

/** @brief Wrapper for a C++ function handle */
class CXXStateFunction : public StateFunction {
public:
  /** @brief C++ std::function wrapper of function handle */
  typedef std::function<bool(const std::string&, yggdrasil_rapidjson::Document&)> FunctionType;
  /** @brief Type for pointer to C++ function handles */
  typedef bool (*FunctionPtr)(const std::string&, yggdrasil_rapidjson::Document&);
  typedef FunctionType* FunctionTypePtr; /**< Pointer to function handle */
  /**
   * @brief Constructor from pointer to function handle
   * @param[in] ptr Pointer to function that should be wrapped.
   */
  YGG_API CXXStateFunction(FunctionTypePtr ptr = nullptr);
  /**
   * @brief Constructor from function handle.
   * @param[in] func Function to wrap.
   */
  YGG_API CXXStateFunction(FunctionType& func);
  /**
   * @brief Constructor from function pointer.
   * @param[in] ptr Pointer to function that should be wrapped.
   */
  YGG_API CXXStateFunction(FunctionPtr ptr);
  /** @brief Destructor */
  YGG_API ~CXXStateFunction() override;
  /**
   * @brief Call the function.
   * @param[in] name Name of state variable/action.
   * @param[in,out] data Value for set/get/action.
   * @return true if successful, false otherwise
   */
  YGG_API bool operator()(const std::string& name,
                          yggdrasil_rapidjson::Document& data) override;
  /** \copydoc StateFunction::copy */
  YGG_API StateFunction* copy() const override;
  /**
   * @brief Copy constructor
   * @param[in] other CXXStateFunction instance to copy.
   */
  YGG_API CXXStateFunction(const CXXStateFunction& other);
  /**
   * @brief Assignment operator
   * @param[in] other CXXStateFunction instance to copy into this one.
   * @returns Updated reference to this instance.
   */
  YGG_API CXXStateFunction& operator=(const CXXStateFunction& other);
private:
  bool _created; /**< Marker for if the function was created */
  FunctionTypePtr _ptr; /**< Pointer to function handle */
};

/** @brief Wrapper for a C function handle */
class CStateFunction : public StateFunction {
public:
  /** @brief C function handle type */
  typedef int (*FunctionPtr)(const char*, generic_t);
  /**
   * @brief Constructor from function pointer
   * @param[in] ptr Pointer to C function handle.
   */
  YGG_API CStateFunction(FunctionPtr ptr);
  /** @brief Destructor */
  YGG_API ~CStateFunction() override;
  /**
   * @brief Call the function.
   * @param[in] name Name of state variable/action.
   * @param[in,out] data Value for set/get/action.
   * @return true if successful, false otherwise
   */
  YGG_API bool operator()(const std::string& name,
                          yggdrasil_rapidjson::Document& data) override;
  /** \copydoc StateFunction::copy */
  YGG_API StateFunction* copy() const override;
  /**
   * @brief Copy constructor
   * @param[in] other CStateFunction instance to copy.
   */
  YGG_API CStateFunction(const CStateFunction& other);
  /**
   * @brief Assignment operator
   * @param[in] other CStateFunction instance to copy into this one.
   * @returns Updated reference to this instance.
   */
  YGG_API CStateFunction& operator=(const CStateFunction& other);
  
private:
  FunctionPtr _ptr; /**< Pointer to function handle */
};

/** @brief Wrapper for an embedded language function handle */
class EmbeddedStateFunction : public StateFunction {
public:
  /** @brief Pointer to embedded function wrapper */
  typedef FunctionWrapper* FunctionPtr;
  /**
     @brief Constructor from an embedded function
     @param[in] func Embedded function.
   */
  YGG_API EmbeddedStateFunction(FunctionWrapper& func);
  /**
     @brief Constructor from an embedded function
     @param[in] ptr Pointer to embedded function.
   */
  YGG_API EmbeddedStateFunction(FunctionPtr ptr);
  /**
     @brief Constructor from an embedded function pointer
     @param[in] ptr Pointer to function in embedded language
     @param[in] language Embedded language of ptr
  */
  YGG_API EmbeddedStateFunction(void* ptr, const LANGUAGE& language);
  /** @brief Destructor */
  YGG_API ~EmbeddedStateFunction() override;
  /**
   * @brief Call the function.
   * @param[in] name Name of state variable/action.
   * @param[in,out] data Value for set/get/action.
   * @return true if successful, false otherwise
   */
  YGG_API bool operator()(const std::string& name,
                          yggdrasil_rapidjson::Document& data) override;
  /** \copydoc StateFunction::copy */
  YGG_API StateFunction* copy() const override;
  /**
   * @brief Copy constructor
   * @param[in] other EmbeddedStateFunction instance to copy.
   */
  YGG_API EmbeddedStateFunction(const EmbeddedStateFunction& other);
  /**
   * @brief Assignment operator
   * @param[in] other EmbeddedStateFunction instance to copy into this one.
   * @returns Updated reference to this instance.
   */
  YGG_API EmbeddedStateFunction& operator=(const EmbeddedStateFunction& other);
private:
  FunctionPtr _ptr; /**< Wrapped function pointer. */
};
  
/**
   @brief Class for interfacing with a model's state.
 */
class StateInterface : public YggInterface::utils::LogBase {
private:
  /**
   * @brief Helper method to wrap different supported function types.
   * @param[in] func Function to wrap.
   * @returns New StateFunction instance wrapping func.
   */
  static StateFunction* _wrap_func(StateFunction* func);
  /** \copydoc StateInterface::_wrap_func */
  static StateFunction* _wrap_func(typename CXXStateFunction::FunctionType& func);
  /** \copydoc StateInterface::_wrap_func */
  static StateFunction* _wrap_func(typename CXXStateFunction::FunctionTypePtr func);
  /** \copydoc StateInterface::_wrap_func */
  static StateFunction* _wrap_func(typename CXXStateFunction::FunctionPtr func);
  /** \copydoc StateInterface::_wrap_func */
  static StateFunction* _wrap_func(typename CStateFunction::FunctionPtr func);
  /** \copydoc StateInterface::_wrap_func */
  static StateFunction* _wrap_func(FunctionWrapper& func);
  /** \copydoc StateInterface::_wrap_func */
  static StateFunction* _wrap_func(typename EmbeddedStateFunction::FunctionPtr func);
  /**
   * @brief Helper method to wrap an embedded function pointer.
   * @param[in] ptr Pointer to embedded function.
   * @param[in] language Language of the embedded function pointer.
   * @returns New StateFunction instance wrapping ptr.
   */
  static StateFunction* _wrap_func(void* ptr, const LANGUAGE& language);
public:
  StateInterface(const StateInterface& other) = delete;
  StateInterface& operator=(const StateInterface&) = delete;
  
  /**
   * @brief Create a state interface from external functions.
   * @tparam Tget Type of the fget function.
   * @tparam Tset Type of the fset function.
   * @tparam Tact Type of the fact function.
   * @param[in] fget Function that should be used to get state variables.
   * @param[in] fset Function that should be used to set state variables.
   * @param[in] fact Function that should be used to perform actions.
   * @param[in] name Name of the server communicator that should be used
   *   to receive requests.
   * @param[in] flags Communicator flags.
   * @param[in] request_commtype Type of communicator to use for requests.
   * @param[in] reply_commtype Type of communicator to use for replies.
  */
  template<typename Tget, typename Tset, typename Tact>
  YGG_API_DEF StateInterface(Tget fget, Tset fset, Tact fact,
                             const std::string& name = "state",
                             FLAG_TYPE flags = 0,
                             const COMM_TYPE request_commtype = DEFAULT_COMM,
                             const COMM_TYPE reply_commtype = DEFAULT_COMM) :
    LogBase(),
    comm(name, flags, SERVER_COMM, 0, request_commtype, reply_commtype),
    _complete(false),
    _get(_wrap_func(fget)),
    _set(_wrap_func(fset)),
    _act(_wrap_func(fact)) {
    comm.addSchema("{\"type\": \"any\"}", false, SEND);
    comm.addSchema("{\"type\": \"any\"}", false, RECV);
  }
  /** @brief Destructor */
  YGG_API ~StateInterface();
  /** \copydoc YggInterface::utils::LogBase::logClass */
  std::string logClass() const override { return "StateInterface"; }
  /** \copydoc YggInterface::utils::LogBase::logInst */
  YGG_API std::string logInst() const override;

  /**
   * @brief Continuous receiving requests until the resume command is
   *   received.
   * @returns true if successful, false otherwise.
   */
  YGG_API virtual bool reply_to_requests();

  /**
   * @brief Get a state variable.
   * @param[in] name State variable name.
   * @param[out] data Destination that state variable should be stored in
   * @returns true if successful, false otherwise.
   */
  YGG_API virtual bool get(const std::string& name,
                           yggdrasil_rapidjson::Document& data);
  
  /**
   * @brief Set a state variable.
   * @param[in] name State variable name.
   * @param[in] data Data that state variable should be set to.
   * @returns true if successful, false otherwise.
   */
  YGG_API virtual bool set(const std::string& name,
                           yggdrasil_rapidjson::Document& data);

  /**
   * @brief Perform an action.
   * @param[in] name Action name.
   * @param[in] param Data that state variable should be set to.
   * @returns true if successful, false otherwise.
   */
  YGG_API virtual bool act(const std::string& name,
                           yggdrasil_rapidjson::Document& param);

private:
  ServerComm comm; /**< Server communicator. */
  bool _complete; /**< Set to true when client requests the simulation run to completion without further stops to interact with the state */
  StateFunction* _get; /**< External function to get a state variable */
  StateFunction* _set; /**< External function to set a state variable */
  StateFunction* _act; /**< External function to act on state */
};

/**
 * @brief Allow other models to set requests to inspect or modify the
 *  state.
 * @tparam Tget Type of the fget function.
 * @tparam Tset Type of the fset function.
 * @tparam Tact Type of the fact function.
 * @param[in] fget Function that should be used to get state variables.
 * @param[in] fset Function that should be used to set state variables.
 * @param[in] fact Function that should be used to perform actions.
 * @param[in] name Name of the communicator to use for requests.
 * @param[in] flags Bit flags to set communicator properties.
 * @param[in] commtype Type of communicator that should be used. Defaults
 *   to DEFAULT_COMM that is set based on the available packages at
 *   compilation.
 * @return true if successful, false otherwise.
 */
template<typename Tget, typename Tset, typename Tact>
YGG_API_DEF bool replyToStateRequests(Tget fget, Tset fset, Tact fact,
                                      const std::string& name="state",
                                      FLAG_TYPE flags = 0,
                                      const COMM_TYPE commtype = DEFAULT_COMM) {
  StateInterface state(fget, fset, fact, name,
                       flags | COMM_FLAG_INTERFACE | COMM_FLAG_GLOBAL,
                       commtype, commtype);
  return state.reply_to_requests();
}
  
}
}
