all : libuv/libuv.a

libuv/libuv.a : libuv/.git Makefile
	make -C libuv

libuv/.git : Makefile
	mkdir -p libuv
	if test -d "libuv/.git"; then \
	  rm libuv -rf; \
	fi;
	git clone https://github.com/joyent/libuv.git

clean :
