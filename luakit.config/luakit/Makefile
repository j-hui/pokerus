type=heads
ref=develop

luakit/doc/luadoc: Makefile clean
	curl -fsSL https://github.com/luakit/luakit/archive/refs/$(type)/$(ref).tar.gz | tar xz
	mv luakit-$(ref) luakit

clean:
	rm -rf luakit
