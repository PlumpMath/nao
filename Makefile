bin_PROGRAMS = nao

nao_SOURCES = src/main.c \
             src/tick.c \
             src/c_main.scm \
             src/c_tick.scm \
             src/c_coroutine.scm \
             src/utils.c \
             src/logger.c

nao_SOURCES_CMP = src/main.c \
             src/tick.c \
             src/utils.c \
             src/logger.c \
             src/c_main.scm

SUBDIRS = deps/libuv

INCLUDES = -Ideps/libuv/include -Isrc -include-path src

LIBS = -Ldeps/libuv -luv -lm -ldl -lrt -lpthread

.PHONY: subdirs

$(bin_PROGRAMS) : $(nao_SOURCES) subdirs Makefile
	csc $(nao_SOURCES_CMP) $(LIBS) $(INCLUDES) -o $@ -embedded


subdirs:
	makedir -p $(SUBDIRS); make -C $(SUBDIRS)

