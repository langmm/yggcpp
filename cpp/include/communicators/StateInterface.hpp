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
   @brief Class for interfacing with a model's state.
 */
class StateInterface : public YggInterface::utils::LogBase {
public:
  /** Type for function handles */
  typedef std::function<bool(const std::string&, yggdrasil_rapidjson::Document&)> StateFunction;
  /** Type for pointer to function handles */
  typedef bool (*StateFunctionRefPtr)(const std::string&, yggdrasil_rapidjson::Document&);
  typedef StateFunction* StateFunctionPtr; /**< Pointer to function handle */
  typedef bool (*CStateFunctionPtr)(const char*, generic_t);
    
  StateInterface(const StateInterface& other) = delete;
  StateInterface& operator=(const StateInterface&) = delete;
  
  /**
     Create a state interface from external functions.
     @param[in] fget Function that should be used to get state variables.
     @param[in] fset Function that should be used to set state variables.
     @param[in] fact Function that should be used to perform actions.
     @param[in] name Name of the server communicator that should be used
       to receive requests.
     @param[in] flags Communicator flags.
     @param[in] request_commtype Type of communicator to use for requests.
     @param[in] reply_commtype Type of communicator to use for replies.
   */
  YGG_API StateInterface(StateFunctionPtr fget = nullptr,
                         StateFunctionPtr fset = nullptr,
                         StateFunctionPtr fact = nullptr,
                         const std::string& name = "state",
                         FLAG_TYPE flags = 0,
                         const COMM_TYPE request_commtype = DEFAULT_COMM,
                         const COMM_TYPE reply_commtype = DEFAULT_COMM);

  /**
     Create a state interface from external functions.
     @param[in] fget Function that should be used to get state variables.
     @param[in] fset Function that should be used to set state variables.
     @param[in] fact Function that should be used to perform actions.
     @param[in] name Name of the server communicator that should be used
       to receive requests.
     @param[in] flags Communicator flags.
     @param[in] request_commtype Type of communicator to use for requests.
     @param[in] reply_commtype Type of communicator to use for replies.
   */
  YGG_API StateInterface(StateFunction& fget,
                         StateFunction& fset,
                         StateFunction& fact,
                         const std::string& name = "state",
                         FLAG_TYPE flags = 0,
                         const COMM_TYPE request_commtype = DEFAULT_COMM,
                         const COMM_TYPE reply_commtype = DEFAULT_COMM);
  /**
     Create a state interface from external functions.
     @param[in] fget Function that should be used to get state variables.
     @param[in] fset Function that should be used to set state variables.
     @param[in] fact Function that should be used to perform actions.
     @param[in] name Name of the server communicator that should be used
       to receive requests.
     @param[in] flags Communicator flags.
     @param[in] request_commtype Type of communicator to use for requests.
     @param[in] reply_commtype Type of communicator to use for replies.
   */
  YGG_API StateInterface(StateFunctionRefPtr fget = nullptr,
                         StateFunctionRefPtr fset = nullptr,
                         StateFunctionRefPtr fact = nullptr,
                         const std::string& name = "state",
                         FLAG_TYPE flags = 0,
                         const COMM_TYPE request_commtype = DEFAULT_COMM,
                         const COMM_TYPE reply_commtype = DEFAULT_COMM);
  /**
     Create a state interface from external C functions.
     @param[in] fget C function that should be used to get state variables.
     @param[in] fset C function that should be used to set state variables.
     @param[in] fact C function that should be used to perform actions.
     @param[in] name Name of the server communicator that should be used
       to receive requests.
     @param[in] flags Communicator flags.
     @param[in] request_commtype Type of communicator to use for requests.
     @param[in] reply_commtype Type of communicator to use for replies.
   */
  YGG_API StateInterface(CStateFunctionPtr fget,
                         CStateFunctionPtr fset,
                         CStateFunctionPtr fact,
                         const std::string& name = "state",
                         FLAG_TYPE flags = 0,
                         const COMM_TYPE request_commtype = DEFAULT_COMM,
                         const COMM_TYPE reply_commtype = DEFAULT_COMM);
  /** @brief Destructor */
  YGG_API ~StateInterface();
  /** \copydoc YggInterface::utils::LogBase::logClass */
  std::string logClass() const override { return "StateInterface"; }
  /** \copydoc YggInterface::utils::LogBase::logInst */
  YGG_API std::string logInst() const override;

  /**
     @brief Continuous receiving requests until the resume command is
       received.
     @returns true if successful, false otherwise.
   */
  YGG_API virtual bool reply_to_requests();

  /**
     @brief Get a state variable.
     @param[in] name State variable name.
     @param[out] data Destination that state variable should be stored in
     @returns true if successful, false otherwise.
   */
  YGG_API virtual bool get(const std::string& name,
                           yggdrasil_rapidjson::Document& data);
  
  /**
     @brief Set a state variable.
     @param[in] name State variable name.
     @param[in] data Data that state variable should be set to.
     @returns true if successful, false otherwise.
   */
  YGG_API virtual bool set(const std::string& name,
                           yggdrasil_rapidjson::Document& data);

  /**
     @brief Perform an action.
     @param[in] name Action name.
     @param[in] param Data that state variable should be set to.
     @returns true if successful, false otherwise.
   */
  YGG_API virtual bool act(const std::string& name,
                           yggdrasil_rapidjson::Document& param);

private:
  static StateFunctionPtr _ensure_ptr(StateFunctionPtr ptr);
  static StateFunctionPtr _ensure_ptr(StateFunctionRefPtr ptr);
  static StateFunctionPtr _ensure_ptr(StateFunction& ptr);
  bool _call_c(CStateFunctionPtr func,
               const std::string& name,
               yggdrasil_rapidjson::Document& data);
  ServerComm comm; /**< Server communicator. */
  bool _created; /**< Marker for if the functions were created */
  StateFunctionPtr _get; /**< External function to get state */
  StateFunctionPtr _set; /**< External function to set state */
  StateFunctionPtr _act; /**< External function to act state */
  CStateFunctionPtr _get_c; /**< External C function to get state */
  CStateFunctionPtr _set_c; /**< External C function to set state */
  CStateFunctionPtr _act_c; /**< External C function to act state */
};

/**
 * @brief Allow other models to set requests to inspect or modify the
 *  state.
 * @tparam T Function type.
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
template<typename T>
YGG_API_DEF bool replyToStateRequests(T fget = nullptr,
                                     T fset = nullptr,
                                     T fact = nullptr,
                                     const std::string& name="state",
                                     FLAG_TYPE flags = 0,
                                     const COMM_TYPE commtype = DEFAULT_COMM) {
  StateInterface state(fget, fset, fact, name,
                       flags | COMM_FLAG_INTERFACE | COMM_FLAG_GLOBAL,
                       commtype);
  return state.reply_to_requests();
}

// /**
//    @brief Wrapper for C function to modify state.
//  */
// class CStateFunction {
// public:
//   typedef bool (*CStateFunctionType)(const char*, generic_t);
//   typedef typename StateInterface::StateFunctionPtr StateFunction;
//   /**
//      @brief Constructor.
//      @param[in] func C function pointer.
//    */
//   YGG_API CStateFunction(CStateFunctionType func);
//   /**
//      @brief Call the wrapped function.
//      @param[in] name Name.
//      @param[out] data Data.
//      @returns true if successful, false otherwise.
//    */
//   YGG_API bool operator()(const std::string& name,
//                           yggdrasil_rapidjson::Document& data);

//   YGG_API StateFunctionPtr getStateFunctionPtr();

// private:
//   CStateFunctionType _func_c; /** C function pointer */
//   StateFunctionPtr _func; /** C++ function pointer */
// };

// class CStateInterface : public StateInterface {
//   typedef bool (*CStateFunctionType)(const char*, generic_t);
// };

}
}
