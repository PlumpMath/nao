#include <uv.h>
#include "tick.h"
#include <chicken.h>

int main(int argc, char **argv){

  uv_loop_t * loop = uv_default_loop();

  init_tick();

  int heap    = 0;
  int stack   = 0;
  int symbols = 0;
  CHICKEN_parse_command_line(argc, argv, &heap, &stack, &symbols);

  CHICKEN_run(C_toplevel);

  return uv_run(loop);

}
