ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

CXXFLAGS := -I$(ERTS_INCLUDE_DIR) -I$(ERL_INTERFACE_INCLUDE_DIR) -Ic_src -D_REENTRANT=PTHREADS# -DLUA_USE_APICHECK
CXXFLAGS += -O3 -finline-functions
CXXFLAGS += -fmax-errors=1 -fPIC -Wall -Wno-unused-function
LDFLAGS := -L$(ERL_INTERFACE_LIB_DIR)
LDLIBS := -lerl_interface -lei -lm -lpthread -llua

SOURCES=$(wildcard c_src/*.c)
OBJECTS=$(patsubst %.c, %.o, $(SOURCES))
OUTFILE=priv/luaport

$(OUTFILE): $(OBJECTS)
	gcc -o $(OUTFILE) $(OBJECTS) $(LDFLAGS) $(LDLIBS)

$(OBJECTS): c_src/%.o: c_src/%.c
	gcc $(CXXFLAGS) -c $< -o $@

clean:
	@rm -f $(OUTFILE) c_src/*.o
