PREFIX ?= /usr/local

all : $(PREFIX)/lib/libuv.so

$(PREFIX)/lib/libuv.so : libuv/.git Makefile
	cp libuv/Makefile libuv/Makefile.tmp
	echo 'libuv.so: $$(OBJS) src/fs-poll.o src/inet.o src/uv-common.o src/unix/ev/ev.o' >> libuv/Makefile.tmp
	echo '	$$(CC) -shared -o $$@ $$^' >> libuv/Makefile.tmp
	make -C libuv CC='gcc -fPIC' -f Makefile.tmp libuv.so
	cp libuv/libuv.so $(PREFIX)/lib

libuv/.git : Makefile
	mkdir -p libuv
	if test -d "libuv/.git"; then \
	  rm libuv -rf; \
	fi;
	git clone https://github.com/joyent/libuv.git

clean :
