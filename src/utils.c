#include "utils.h"
#include <uv.h>

#define ERR_MSG_LEN 1024

static char err_msg[ERR_MSG_LEN];

const char * uv_error_msg(){
  return uv_strerror(uv_last_error(uv_default_loop()));
}

char * chicken_error_msg(){
  CHICKEN_get_error_message(&err_msg, ERR_MSG_LEN);
  return err_msg;
}
