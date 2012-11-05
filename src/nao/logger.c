#include "logger.h"
#include <stdio.h>

void info(char * msg){
  printf("[nao INFO]: %s\n", msg);
}

void err(char * msg){
  printf("[nao ERR]: %s\n", msg);
}

void warn(char * msg){
  printf("[nao WARN]: %s\n", msg);
}

void debug(char * msg){
  printf("[nao DEBUG]: %s\n", msg);
}
