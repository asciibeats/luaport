#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <ei.h>

#ifndef LUAP_BUFFER
  #define LUAP_BUFFER 512
#endif

#define LUAP_PACKET 4
#define LUAP_LIBNAME "luaport"
#define LUAP_MTYPE "_mtype"
#define LUAP_TMAP 0
#define LUAP_TTUPLE 1
#define LUAP_TLIST 2

#define EXIT_FAIL_READ 200
#define EXIT_FAIL_WRITE 201
#define EXIT_FAIL_SIZE 202
#define EXIT_BAD_VERSION 210
#define EXIT_BAD_TUPLE 211
#define EXIT_BAD_ATOM 212
#define EXIT_BAD_FUNC 213
#define EXIT_BAD_ARGS 214
#define EXIT_BAD_CALL 215
#define EXIT_BAD_COMMAND 216
#define EXIT_CALL_READ 220
#define EXIT_CALL_VERSION 221
#define EXIT_CALL_RESULT 222

static size_t read4(const unsigned char *buf)
{
  return buf[0] << 24 | buf[1] << 16 | buf[2] << 8 | buf[3];
}

static void write4(char *buf, int val)
{
  buf[0] = (val >> 24) & 0xff;
  buf[1] = (val >> 16) & 0xff;
  buf[2] = (val >> 8) & 0xff;
  buf[3] = val & 0xff;
}

static size_t swap4(size_t val)
{
  return (val & 0xff000000) >> 24 | (val & 0x00ff0000) >> 8 | (val & 0x0000ff00) << 8 | (val & 0x000000ff) << 24;
}

static int read_bytes(char *buf, int len)
{
  ssize_t s, c = 0;

  do
  {
    s = read(STDIN_FILENO, buf + c, len - c);

    if (s < 0)
    {
      exit(EXIT_FAIL_READ);
    }
    else if (s == 0)
    {
      return 0;
    }

    c += s;
  }
  while (c < len);

  return len;
}

static int write_bytes(char *buf, int len)
{
  ssize_t s, c = 0;

  do
  {
    s = write(STDOUT_FILENO, buf + c, len - c);

    if (s < 0)
    {
      exit(EXIT_FAIL_WRITE);
    }
    else if (s == 0)
    {
      return 0;
    }

    c += s;
  }
  while (c < len);

  return len;
}

static int read_term(char *buf, int *index)
{
  if (read_bytes(buf, LUAP_PACKET) != LUAP_PACKET)
  {
    return 0;
  }

  size_t len = read4((unsigned char *)buf);

  if (len > LUAP_BUFFER)
  {
    exit(EXIT_FAIL_SIZE);
  }

  *index = 0;
  return read_bytes(buf, len);
}

static int write_term(ei_x_buff *eb)
{
  size_t len = swap4(eb->index);

  if (write_bytes((char *)&len, LUAP_PACKET) != LUAP_PACKET)
  {
    return 0;
  }

  len = write_bytes(eb->buff, eb->index);
  eb->index = 0;
  return len;
}

static void luap_setmetatype(lua_State *L, int index, int type)
{
  if (type == LUAP_TMAP)
  {
    if (lua_getmetatable(L, index))
    {
      lua_pushfstring(L, LUAP_MTYPE);
      lua_pushnil(L);
      lua_rawset(L, -3);
      lua_pop(L, 1);
    }
  }
  else
  {
    index = lua_absindex(L, index);

    if (!lua_getmetatable(L, index))
    {
      lua_createtable(L, 0, 1);
    }

    lua_pushfstring(L, LUAP_MTYPE);
    lua_pushinteger(L, type);
    lua_rawset(L, -3);
    lua_setmetatable(L, index);
  }
}

static int luap_hasmetatype(lua_State *L, int index, int type)
{
  if (!lua_istable(L, index))
  {
    return 0;
  }

  if (lua_getmetatable(L, index))
  {
    lua_pushstring(L, LUAP_MTYPE);

    if (lua_rawget(L, -2) != LUA_TNIL)
    {
      return type == (int)lua_tointeger(L, -1);
    }
    else
    {
      return type == LUAP_TMAP;
    }
  }
  else
  {
    return type == LUAP_TMAP;
  }
}

static int e2l_any(const char *buf, int *index, lua_State *L);

static int e2l_integer(const char *buf, int *index, lua_State *L)
{
  long l;

  if (ei_decode_long(buf, index, &l))
  {
    return -1;
  }

  lua_pushinteger(L, l);
  return 0;
}

static int e2l_float(const char *buf, int *index, lua_State *L)
{
  double d;

  if (ei_decode_double(buf, index, &d))
  {
    return -1;
  }

  lua_pushnumber(L, d);
  return 0;
}

static int e2l_binary(const char *buf, int *index, lua_State *L)
{
  int type;
  long size = 0;

  ei_get_type(buf, index, &type, (int *)&size);
  char s[size];

  if (ei_decode_binary(buf, index, s, &size))
  {
    return -1;
  }

  lua_pushlstring(L, s, size);
  return 0;
}

static int e2l_atom(const char *buf, int *index, lua_State *L)
{
  char a[MAXATOMLEN];

  if (ei_decode_atom(buf, index, a))
  {
    return -1;
  }

  if (!strcmp(a, "true"))
  {
    lua_pushboolean(L, 1);
  }
  else if (!strcmp(a, "false"))
  {
    lua_pushboolean(L, 0);
  }
  else if (!strcmp(a, "undefined"))
  {
    lua_pushnil(L);
  }
  else
  {
    lua_pushstring(L, a);
  }

  return 0;
}

static int e2l_list(const char *buf, int *index, lua_State *L)
{
  int arity;

  if (ei_decode_list_header(buf, index, &arity))
  {
    return -1;
  }

  lua_createtable(L, arity, 0);
  luap_setmetatype(L, -1, LUAP_TLIST);

  for (int i = 1; i <= arity; i++)
  {
    e2l_any(buf, index, L);
    lua_rawseti(L, -2, i);
  }
  
  ei_skip_term(buf, index);
  return 0;
}

static int e2l_string(const char *buf, int *index, lua_State *L)
{
  int type, size;
  ei_get_type(buf, index, &type, &size);
  char s[size + 1];

  if (ei_decode_string(buf, index, s))
  {
    return -1;
  }

  lua_createtable(L, size, 0);
  luap_setmetatype(L, -1, LUAP_TLIST);

  for (int i = 0; i < size; i++)
  {
    lua_pushinteger(L, (unsigned char)s[i]);
    lua_rawseti(L, -2, i + 1);
  }

  return 0;
}

static int e2l_nil(const char *buf, int *index, lua_State *L)
{
  lua_createtable(L, 0, 0);
  luap_setmetatype(L, -1, LUAP_TLIST);
  ei_skip_term(buf, index);
  return 0;
}

static int e2l_tuple(const char *buf, int *index, lua_State *L)
{
  int arity;

  if (ei_decode_tuple_header(buf, index, &arity))
  {
    return -1;
  }

  lua_createtable(L, arity, 0);
  luap_setmetatype(L, -1, LUAP_TTUPLE);

  for (int i = 1; i <= arity; i++)
  {
    e2l_any(buf, index, L);
    lua_rawseti(L, -2, i);
  }

  return 0;
}

static int e2l_map(const char *buf, int *index, lua_State *L)
{
  int arity;

  if (ei_decode_map_header(buf, index, &arity))
  {
    return -1;
  }

  lua_createtable(L, 0, arity);

  for (int i = 0; i < arity; i++)
  {
    e2l_any(buf, index, L);
    e2l_any(buf, index, L);
    lua_rawset(L, -3);
  }

  return 0;
}

/*static int e2l_ref(const char *buf, int *index, lua_State *L)
{
  erlang_ref *ref = lua_newuserdata(L, sizeof(erlang_ref));

  int type, size;
  ei_get_type(buf, index, &type, &size);

  if (ei_decode_ref(buf, index, ref))
  {
    return -1;
  }

  return 0;
}*/

static int e2l_any(const char *buf, int *index, lua_State *L)
{
  int type, size;
  ei_get_type(buf, index, &type, &size);
  
  switch (type)
  {
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
      return e2l_integer(buf, index, L);
    case ERL_FLOAT_EXT:
      return e2l_float(buf, index, L);
    case ERL_BINARY_EXT:
      return e2l_binary(buf, index, L);
    case ERL_ATOM_EXT:
      return e2l_atom(buf, index, L);
    case ERL_LIST_EXT:
      return e2l_list(buf, index, L);
    case ERL_STRING_EXT:
      return e2l_string(buf, index, L);
    case ERL_NIL_EXT:
      return e2l_nil(buf, index, L);
    case ERL_SMALL_TUPLE_EXT:
    case ERL_LARGE_TUPLE_EXT:
      return e2l_tuple(buf, index, L);
    case ERL_MAP_EXT:
      return e2l_map(buf, index, L);
    /*case ERL_REFERENCE_EXT:
      return e2l_ref(buf, index, L);*/
    default:
      return -1;
  }

  return 0;
}

static int e2l_args(const char *buf, int *index, lua_State *L, int *nargs)
{
  int type;
  ei_get_type(buf, index, &type, nargs);

  if (type == ERL_LIST_EXT)
  {
    if (ei_decode_list_header(buf, index, nargs))
    {
      return -1;
    }

    for (int i = 0; i < *nargs; i++)
    {
      e2l_any(buf, index, L);
    }

    ei_skip_term(buf, index);
    return 0;
  }
  else if (type == ERL_STRING_EXT)
  {
    char s[*nargs + 1];

    if (ei_decode_string(buf, index, s))
    {
      return -1;
    }
    
    for (int i = 0; i < *nargs; i++)
    {
      lua_pushinteger(L, (unsigned char)s[i]);
    }

    return 0;
  }
  else if (type == ERL_NIL_EXT)
  {
    ei_skip_term(buf, index);
    return 0;
  }

  return -1;
}

static int e2l_call(const char *buf, int *index, lua_State *L, int *nargs)
{
  long ref;

  if (ei_decode_long(buf, index, &ref))
  {
    return -1;
  }

  lua_rawgeti(L, LUA_REGISTRYINDEX, abs(ref));

  if (ref > 0)
  {
    luaL_unref(L, LUA_REGISTRYINDEX, ref);
  }

  int top = lua_gettop(L);
  *nargs = (int)luaL_len(L, top);

  for (int i = 1; i <= *nargs; i++)
  {
    lua_rawgeti(L, top, i);
  }

  (*nargs)--;
  lua_remove(L, top);
  return 0;
}

static void l2e_any(lua_State *L, int index, ei_x_buff *eb);

static void l2e_integer(lua_State *L, int index, ei_x_buff *eb)
{
  lua_Integer i = lua_tointeger(L, index);
  ei_x_encode_long(eb, i);
}

static void l2e_number(lua_State *L, int index, ei_x_buff *eb)
{
  lua_Number n = lua_tonumber(L, index);
  ei_x_encode_double(eb, n);
}

static void l2e_string_string(lua_State *L, int index, ei_x_buff *eb)
{
  const char *s = lua_tostring(L, index);
  ei_x_encode_string(eb, s);
}

static void l2e_string_binary(lua_State *L, int index, ei_x_buff *eb)
{
  size_t len;
  const char *s = lua_tolstring(L, index, &len);
  ei_x_encode_binary(eb, s, len);
}

static void l2e_string_atom(lua_State *L, int index, ei_x_buff *eb)
{
  const char *s = lua_tostring(L, index);
  ei_x_encode_atom(eb, s);
}

static void l2e_table_list(lua_State *L, int index, ei_x_buff *eb)
{
  int arity = (int)luaL_len(L, index);

  if (arity > 0)
  {
    ei_x_encode_list_header(eb, arity);

    for (int i = 1; i <= arity; i++)
    {
      lua_rawgeti(L, index, i);
      l2e_any(L, -1, eb);
      lua_pop(L, 1);
    }
  }

  ei_x_encode_empty_list(eb);
}

static void l2e_table_tuple(lua_State *L, int index, ei_x_buff *eb)
{
  int arity = (int)luaL_len(L, index);
  ei_x_encode_tuple_header(eb, arity);

  for (int i = 1; i <= arity; i++)
  {
    lua_rawgeti(L, index, i);
    l2e_any(L, -1, eb);
    lua_pop(L, 1);
  }
}

static void l2e_table_map(lua_State *L, int index, ei_x_buff *eb)
{
  index = lua_absindex(L, index);
  int eb_index = eb->index + 1;

  char b[5];
  b[0] = ERL_MAP_EXT;
  ei_x_append_buf(eb, b, 5);

  lua_pushnil(L);
  int arity = 0;

  while (lua_next(L, index))
  {
    l2e_any(L, -2, eb);
    l2e_any(L, -1, eb);
    lua_pop(L, 1);
    arity++;
  }

  write4(eb->buff + eb_index, arity);
}

static void l2e_table(lua_State *L, int index, ei_x_buff *eb)
{
  if (luaL_getmetafield(L, index, LUAP_MTYPE) == LUA_TNUMBER)
  {
    int mtype = (int)lua_tointeger(L, -1);
    lua_pop(L, 1);

    if (mtype == LUAP_TLIST)
    {
      l2e_table_list(L, index, eb);
    }
    else if (mtype == LUAP_TTUPLE)
    {
      l2e_table_tuple(L, index, eb);
    }
    else
    {
      l2e_table_map(L, index, eb);
    }
  }
  else
  {
    l2e_table_map(L, index, eb);
  }
}

static void l2e_boolean(lua_State *L, int index, ei_x_buff *eb)
{
  int boolean = lua_toboolean(L, index);
  ei_x_encode_boolean(eb, boolean);
}

/*static void l2e_userdata(lua_State *L, int index, ei_x_buff *eb)
{
  const erlang_ref *ref = lua_touserdata(L, index);
  ei_x_encode_ref(eb, ref);
}*/

static void l2e_any(lua_State *L, int index, ei_x_buff *eb)
{
  switch (lua_type(L, index))
  {
    case LUA_TNUMBER:
      if (lua_isinteger(L, index))
      {
        l2e_integer(L, index, eb);
      }
      else
      {
        l2e_number(L, index, eb);
      }
      break;
    case LUA_TSTRING:
      l2e_string_binary(L, index, eb);
      break;
    case LUA_TTABLE:
      l2e_table(L, index, eb);
      break;
    case LUA_TBOOLEAN:
      l2e_boolean(L, index, eb);
      break;
    case LUA_TNIL:
      ei_x_encode_atom(eb, "undefined");
      break;
    /*case LUA_TUSERDATA:
      l2e_userdata(L, index, eb);
      break;*/
    default:
      luaL_error(L, "unsupported type");
  }
}

static void l2e_args(lua_State *L, int index, ei_x_buff *eb)
{
  int top = lua_gettop(L);
  int arity = top - index + 1;

  if (arity > 0)
  {
    ei_x_encode_list_header(eb, arity);

    for (int i = index; i <= top; i++)
    {
      l2e_any(L, i, eb);
    }
  }

  ei_x_encode_empty_list(eb);
}

static void l2e_call(lua_State *L, int index, ei_x_buff *eb)
{
  luaL_checktype(L, index, LUA_TFUNCTION);

  int top = lua_gettop(L);
  int arity = top - index + 1;

  lua_createtable(L, arity, 0);
  lua_insert(L, index);

  for (int i = arity; i >= 1; i--)
  {
    lua_rawseti(L, index, i);
  }

	int ref = luaL_ref(L, LUA_REGISTRYINDEX);
  lua_pushinteger(L, ref);
  ei_x_encode_long(eb, ref);
}

static void l2e_error(lua_State *L, int index, ei_x_buff *eb)
{
  ei_x_encode_version(eb);
  ei_x_encode_tuple_header(eb, 2);
  ei_x_encode_atom(eb, "error");
  l2e_string_string(L, index, eb);
}

static void l2e_ok(lua_State *L, int index, ei_x_buff *eb)
{
  ei_x_encode_version(eb);
  ei_x_encode_tuple_header(eb, 2);
  ei_x_encode_atom(eb, "ok");
  l2e_args(L, index, eb);
}

static int luaport_call(lua_State *L)
{
  ei_x_buff *eb = lua_touserdata(L, lua_upvalueindex(2));
  char *buf = lua_touserdata(L, lua_upvalueindex(3));
  int *index = lua_touserdata(L, lua_upvalueindex(4));

  ei_x_encode_version(eb);
  ei_x_encode_tuple_header(eb, 3);
  ei_x_encode_atom(eb, "call");

  l2e_string_atom(L, lua_upvalueindex(1), eb);
  l2e_args(L, 1, eb);

  write_term(eb);

  if (read_term(buf, index) <= 0)
  {
    exit(EXIT_CALL_READ);
  }

  int version;

  if (ei_decode_version(buf, index, &version))
  {
    exit(EXIT_CALL_VERSION);
  }

  int nargs;

  if (e2l_args(buf, index, L, &nargs))
  {
    exit(EXIT_CALL_RESULT);
  }

  return nargs;
}

static int luaport_call_index(lua_State *L)
{
  lua_pushvalue(L, lua_upvalueindex(1));
  lua_pushvalue(L, lua_upvalueindex(2));
  lua_pushvalue(L, lua_upvalueindex(3));
  lua_pushcclosure(L, &luaport_call, 4);
  return 1;
}

static int luaport_cast(lua_State *L)
{
  ei_x_buff *eb = lua_touserdata(L, lua_upvalueindex(2));

  ei_x_encode_version(eb);
  ei_x_encode_tuple_header(eb, 3);
  ei_x_encode_atom(eb, "cast");

  l2e_string_atom(L, lua_upvalueindex(1), eb);
  l2e_args(L, 1, eb);

  write_term(eb);
  return 0;
}

static int luaport_cast_index(lua_State *L)
{
  lua_pushvalue(L, lua_upvalueindex(1));
  lua_pushcclosure(L, &luaport_cast, 2);
  return 1;
}

static int luaport_after(lua_State *L)
{
  ei_x_buff *eb = lua_touserdata(L, lua_upvalueindex(1));

  ei_x_encode_version(eb);
  ei_x_encode_tuple_header(eb, 3);
  ei_x_encode_atom(eb, "after");

  l2e_integer(L, 1, eb);
  l2e_call(L, 2, eb);

  write_term(eb);
  return 1;
}

static int luaport_interval(lua_State *L)
{
  ei_x_buff *eb = lua_touserdata(L, lua_upvalueindex(1));

  ei_x_encode_version(eb);
  ei_x_encode_tuple_header(eb, 3);
  ei_x_encode_atom(eb, "interval");

  l2e_integer(L, 1, eb);
  l2e_call(L, 2, eb);

  write_term(eb);
  return 1;
}

static int luaport_cancel(lua_State *L)
{
  lua_Integer ref = luaL_checkinteger(L, 1);

  if (lua_rawgeti(L, LUA_REGISTRYINDEX, ref) == LUA_TTABLE)
  {
    luaL_unref(L, LUA_REGISTRYINDEX, ref);
    ei_x_buff *eb = lua_touserdata(L, lua_upvalueindex(1));

    ei_x_encode_version(eb);
    ei_x_encode_tuple_header(eb, 2);
    ei_x_encode_atom(eb, "cancel");
    ei_x_encode_long(eb, ref);

    write_term(eb);
  }

  return 0;
}

static int luaport_print(lua_State *L)
{
  ei_x_buff *eb = lua_touserdata(L, lua_upvalueindex(1));

  ei_x_encode_version(eb);
  ei_x_encode_tuple_header(eb, 2);
  ei_x_encode_atom(eb, "info");

  l2e_args(L, 1, eb);

  write_term(eb);
  return 0;
}

static int luaport_aslist(lua_State *L)
{
  luaL_checktype(L, 1, LUA_TTABLE);
  luap_setmetatype(L, 1, LUAP_TLIST);
  return 1;
}

static int luaport_astuple(lua_State *L)
{
  luaL_checktype(L, 1, LUA_TTABLE);
  luap_setmetatype(L, 1, LUAP_TTUPLE);
  return 1;
}

static int luaport_asmap(lua_State *L)
{
  luaL_checktype(L, 1, LUA_TTABLE);
  luap_setmetatype(L, 1, LUAP_TMAP);
  return 1;
}

static int luaport_islist(lua_State *L)
{
  lua_pushboolean(L, luap_hasmetatype(L, 1, LUAP_TLIST));
  return 1;
}

static int luaport_istuple(lua_State *L)
{
  lua_pushboolean(L, luap_hasmetatype(L, 1, LUAP_TTUPLE));
  return 1;
}

static int luaport_ismap(lua_State *L)
{
  lua_pushboolean(L, luap_hasmetatype(L, 1, LUAP_TMAP));
  return 1;
}

static const struct luaL_Reg luaport_func[] = {
  {"aslist", luaport_aslist},
  {"astuple", luaport_astuple},
  {"asmap", luaport_asmap},
  {"islist", luaport_islist},
  {"istuple", luaport_istuple},
  {"ismap", luaport_ismap},
  {NULL, NULL}
};

static int luaopen_luaport(lua_State *L)
{
  ei_x_new(lua_newuserdata(L, sizeof(ei_x_buff)));
  lua_newuserdata(L, LUAP_BUFFER);
  lua_newuserdata(L, sizeof(int));

  luaL_newlib(L, luaport_func);

  lua_newtable(L);
  lua_createtable(L, 0, 1);
  lua_pushvalue(L, 2);
  lua_pushvalue(L, 3);
  lua_pushvalue(L, 4);
  lua_pushcclosure(L, luaport_call_index, 3);
  lua_setfield(L, -2, "__index");
  lua_setmetatable(L, -2);
  lua_setfield(L, -2, "call");

  lua_newtable(L);
  lua_createtable(L, 0, 1);
  lua_pushvalue(L, 2);
  lua_pushcclosure(L, luaport_cast_index, 1);
  lua_setfield(L, -2, "__index");
  lua_setmetatable(L, -2);
  lua_setfield(L, -2, "cast");

  lua_pushvalue(L, 2);
  lua_pushcclosure(L, luaport_after, 1);
  lua_setfield(L, -2, "after");

  lua_pushvalue(L, 2);
  lua_pushcclosure(L, luaport_interval, 1);
  lua_setfield(L, -2, "interval");

  lua_pushvalue(L, 2);
  lua_pushcclosure(L, luaport_cancel, 1);
  lua_setfield(L, -2, "cancel");

  lua_pushvalue(L, 2);
  lua_pushcclosure(L, luaport_print, 1);
  lua_setglobal(L, "print");

  return 1;
}

int main(int argc, char *argv[])
{
  int version;
  int type;
  int arity;
  char func[MAXATOMLEN];
  int nargs;

  char buf[LUAP_BUFFER];
  int index;
  ei_x_buff eb;
  ei_x_new(&eb);

  lua_State *L = luaL_newstate();
  luaL_requiref(L, "_G", luaopen_base, 1);
  luaL_requiref(L, LUA_MATHLIBNAME, luaopen_math, 1);
  luaL_requiref(L, LUA_TABLIBNAME, luaopen_table, 1);
  luaL_requiref(L, LUA_STRLIBNAME, luaopen_string, 1);
  luaL_requiref(L, LUA_OSLIBNAME, luaopen_os, 1);
  luaL_requiref(L, LUA_DBLIBNAME, luaopen_debug, 1);
  luaL_requiref(L, LUA_LOADLIBNAME, luaopen_package, 1);
  luaL_requiref(L, LUAP_LIBNAME, luaopen_luaport, 1);
  lua_settop(L, 0);

  if (luaL_dofile(L, "main.lua"))
  {
    l2e_error(L, 1, &eb);
  }
  else
  {
    l2e_ok(L, 1, &eb);
  }

  write_term(&eb);
  lua_settop(L, 0);

  while (read_term(buf, &index) > 0)
  {
    if (ei_decode_version(buf, &index, &version))
    {
      exit(EXIT_BAD_VERSION);
    }
    
    ei_get_type(buf, &index, &type, &arity);

    if (type == ERL_SMALL_TUPLE_EXT)
    {
      if (arity != 2 || ei_decode_tuple_header(buf, &index, NULL))
      {
        exit(EXIT_BAD_TUPLE);
      }

      if (ei_decode_atom(buf, &index, func))
      {
        exit(EXIT_BAD_ATOM);
      }

      if (lua_getglobal(L, func) != LUA_TFUNCTION)
      {
        exit(EXIT_BAD_FUNC);
      }

      if (e2l_args(buf, &index, L, &nargs))
      {
        exit(EXIT_BAD_ARGS);
      }
    }
    else if (type == ERL_SMALL_INTEGER_EXT || type == ERL_INTEGER_EXT)
    {
      if (e2l_call(buf, &index, L, &nargs))
      {
        exit(EXIT_BAD_CALL);
      }
    }
    else
    {
      exit(EXIT_BAD_COMMAND);
    }

    if (lua_pcall(L, nargs, LUA_MULTRET, 0))
    {
      l2e_error(L, 1, &eb);
    }
    else
    {
      l2e_ok(L, 1, &eb);
    }

    write_term(&eb);
    lua_settop(L, 0);
  }

  lua_close(L);
  ei_x_free(&eb);

  return 0;
}
