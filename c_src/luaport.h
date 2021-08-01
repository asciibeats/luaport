#ifndef luaport_h
#define luaport_h

#include "lua.h"

#define LUA_PORTLIBNAME "port"
LUALIB_API int luaopen_port(lua_State *L);

#endif
