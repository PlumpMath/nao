bin_PROGRAMS = nao

nao_DEPS = src/main.c \
           src/tick.c \
           src/c_main.scm \
           src/c_tick.scm \
           src/c_coroutine.scm \
           src/c_event.scm \
           src/c_socket.scm \
           src/c_channel.scm \
           src/c_utils.scm \
           src/c_logger.scm \
           src/c_object.scm \
           src/c_dsl.scm \
           src/c_remote.scm \
           src/socket.c \
           src/utils.c

nao_SOURCES = src/main.c \
              src/tick.c \
              src/utils.c \
              src/socket.c \
              src/c_main.scm

SUBDIRS = deps

INCLUDES = -Ideps/libuv/include -Isrc -include-path src

LIBS = -Ldeps/libuv -luv -lm -ldl -lrt

.PHONY: subdirs

all : subdirs $(bin_PROGRAMS)

$(bin_PROGRAMS) : $(nao_DEPS) Makefile
	csc $(nao_SOURCES) $(LIBS) $(INCLUDES) -o $@ -embedded -:w


subdirs:
	mkdir -p $(SUBDIRS); make -C $(SUBDIRS)

