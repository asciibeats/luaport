ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s erlang halt)
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)])." -s erlang halt)
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)])." -s erlang halt)

# !!! SWAP THESE LINES TO USE LUA 5.4
LUA_CFLAGS ?= -DLUAP_JIT $(shell pkg-config --cflags luajit)
LUA_LDFLAGS ?= $(shell pkg-config --libs luajit)
#LUA_CFLAGS ?= $(shell pkg-config --cflags lua54)
#LUA_LDFLAGS ?= $(shell pkg-config --libs lua54)

DEFINES := -D_REENTRANT=PTHREADS
#DEFINES += -DLUA_USE_APICHECK
DEFINES += -DLUAP_BUFLEN=2048
#DEFINES += -DLUAP_USERTUPLE # encode tuples as lua userdata
#DEFINES += -DLUAP_NOINT # convert floats that are almost integers

# !!! TOGGLE LUA MODULES
DEFINES += -DLUAP_IO # enable lua io
DEFINES += -DLUAP_OS # enable lua os
DEFINES += -DLUAP_DEBUG # enable lua debug
DEFINES += -DLUAP_BIT # enable luajit bit
DEFINES += -DLUAP_FFI # enable luajit ffi

CFLAGS := -Ic_src $(LUA_CFLAGS) -I$(ERTS_INCLUDE_DIR) -I$(ERL_INTERFACE_INCLUDE_DIR)
CFLAGS += -O3 -fPIC
#CFLAGS += -Og -ggdb
CFLAGS += -Wall -Werror -Wno-unused-function
CFLAGS += $(DEFINES)
LDFLAGS := -lpthread -lei $(LUA_LDFLAGS) -L$(ERL_INTERFACE_LIB_DIR)

SOURCES=$(wildcard c_src/*.c)
OBJECTS=$(patsubst %.c, %.o, $(SOURCES))
OUTFILE=priv/luaport

$(OUTFILE): $(OBJECTS)
	gcc -o $(OUTFILE) $(OBJECTS) $(LDFLAGS)

$(OBJECTS): c_src/%.o: c_src/%.c
	gcc $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OUTFILE) c_src/*.o
