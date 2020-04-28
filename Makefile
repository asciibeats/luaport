LUA_CFLAGS ?= $(shell pkg-config --cflags luajit)
LUA_LDFLAGS ?= $(shell pkg-config --libs luajit)

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

LUAP_BUFFER ?= 2048
LUAP_NOINT ?= 1

DEFINES := -D_REENTRANT=PTHREADS
DEFINES += -D_GNU_SOURCE
DEFINES += -DLUAP_BUFFER=$(LUAP_BUFFER)
#DEFINES += -DLUAP_NOINT=$(LUAP_NOINT)
#DEFINES += -DLUA_USE_APICHECK

CFLAGS := -Ic_src $(LUA_CFLAGS) -I$(ERTS_INCLUDE_DIR) -I$(ERL_INTERFACE_INCLUDE_DIR) $(DEFINES)
CFLAGS += -O3 -fPIC
#CFLAGS += -Og -ggdb
CFLAGS += -Wall -Werror -Wno-unused-function -fmax-errors=1
LDFLAGS := -lpthread $(LUA_LDFLAGS) -L$(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei

SOURCES=$(wildcard c_src/*.c)
OBJECTS=$(patsubst %.c, %.o, $(SOURCES))
OUTFILE=priv/luaport

$(OUTFILE): $(OBJECTS)
	gcc -o $(OUTFILE) $(OBJECTS) $(LDFLAGS)

$(OBJECTS): c_src/%.o: c_src/%.c
	gcc $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OUTFILE) c_src/*.o
