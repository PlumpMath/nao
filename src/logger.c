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


#include "logger.h"
#include <stdio.h>
#include <assert.h>

void info(const char * msg){
  printf("[INFO]: %s\n", msg);
}

void err(const char * msg){
  printf("[ERR]: %s\n", msg);
  assert(0);
}

void debug(const char * msg){
  printf("[DEBUG]: %s\n", msg);
}
