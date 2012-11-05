PREFIX ?= /usr/local

bin_PROGRAMS = nao

nao_DEPS = src/main.c \
           src/c_main.scm \
           src/nao/nao.scm \
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
           src/nao/c_signal.scm \
           src/nao/c_timer.scm \
           src/nao/tick.c \
           src/nao/timer.c \
           src/nao/socket.c \
           src/nao/utils.c \
           src/nao/logger.c \
           src/nao/tick.h \
           src/nao/timer.h \
           src/nao/socket.h \
           src/nao/utils.h \
           src/nao/logger.h

nao_SOURCES = src/main.c \
              src/c_main.scm

SUBDIRS = deps

STEM = $(shell pwd)

INCLUDES = -I$(STEM)/deps/libuv/include -Isrc -Isrc/nao -include-path src

LIBS = -L$(PREFIX)/lib -luv

.PHONY: subdirs

all : subdirs $(bin_PROGRAMS)

$(bin_PROGRAMS) : $(nao_DEPS) Makefile
	cd src/nao; CSC_OPTIONS='$(LIBS) $(INCLUDES)' chicken-install
	csc $(nao_SOURCES) $(LIBS) $(INCLUDES) -o $@ -embedded -:w -L '-pthread -lrt'


subdirs:
	mkdir -p $(SUBDIRS); make -C $(SUBDIRS) PREFIX=$(PREFIX)

