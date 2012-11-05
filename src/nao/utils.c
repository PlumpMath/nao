/*
* Copyright 2012 The Nao Authors. All Rights Reserved.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
*/


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
