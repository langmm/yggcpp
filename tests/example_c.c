#include <stdio.h>
#include <string.h>
#include "YggInterface.h"

#ifdef __cplusplus
extern "C" {
#endif

bool example_model_function(const generic_t data_send,
			    generic_t data_recv) {
  printf("IN example_model_function: \n");
  display_generic(data_send);
  /* if (!generic_is_string(data_send)) */
  /*   return false; */
  generic_set_integer(data_recv, strlen(generic_get_string(data_send)));
  return true;
}

#ifdef __cplusplus
}
#endif
