
// LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
//====================================================================
#include "utils/rapidjson_wrapper2.hpp"
template<typename T>
WrapperBase<T>::~WrapperBase() {
  if (created_val)
    delete val_;
  val_ = nullptr;
}