bin_PROGRAMS = nao

nao_DEPS = src/main.c \
           src/c_main.scm \
           src/nao/nao.scm \
           src/nao/tick.c \
           src/nao/c_tick.scm \
           src/nao/c_coroutine.scm \
           src/nao/c_event.scm \
           src/nao/c_socket.scm \
           src/nao/c_channel.scm \
           src/nao/c_utils.scm \
           src/nao/c_logger.scm \
           src/nao/c_object.scm \
           src/nao/c_dsl.scm \
           src/nao/c_remote.scm \
           src/nao/socket.c \
           src/nao/utils.c

nao_SOURCES = src/main.c \
              src/c_main.scm
        #      src/tick.c \
              src/utils.c \
              src/socket.c \
              src/c_main.scm

SUBDIRS = deps

STEM = $(PWD)

INCLUDES = -I$(STEM)/deps/libuv/include -Isrc -Isrc/nao -include-path src

LIBS = -L$(STEM)/deps/libuv -lm -ldl -lrt -luv

.PHONY: subdirs

all : subdirs $(bin_PROGRAMS)

$(bin_PROGRAMS) : $(nao_DEPS) Makefile
	cd src/nao; CSC_OPTIONS='$(LIBS) $(INCLUDES)' chicken-install
	csc $(nao_SOURCES) $(LIBS) $(INCLUDES) -o $@ -embedded -:w


subdirs:
	mkdir -p $(SUBDIRS); make -C $(SUBDIRS)

