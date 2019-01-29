ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

CFLAGS := -I$(ERTS_INCLUDE_DIR) -I$(ERL_INTERFACE_INCLUDE_DIR)
CFLAGS += -Ic_src -D_REENTRANT=PTHREADS# -DLUA_USE_APICHECK
CFLAGS += -O3 -finline-functions# -DLUAP_UNDEFINED_AS_NIL
CFLAGS += -fmax-errors=1 -fPIC -Wall -Wno-unused-function
LDLIBS := -L$(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei -lm -lpthread -llua

SOURCES=$(wildcard c_src/*.c)
OBJECTS=$(patsubst %.c, %.o, $(SOURCES))
OUTFILE=priv/luaport

$(OUTFILE): $(OBJECTS)
	gcc -o $(OUTFILE) $(OBJECTS) $(LDLIBS)

$(OBJECTS): c_src/%.o: c_src/%.c
	gcc $(CFLAGS) -c $< -o $@

clean:
	@rm -f $(OUTFILE) c_src/*.o
