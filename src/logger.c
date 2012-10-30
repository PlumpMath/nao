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
