#include <ei.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#ifdef __cplusplus
	#include <lua.hpp>
#else
	#include <lua.h>
	#include <lualib.h>
	#include <lauxlib.h>
#endif

#define LUAP_PORTLIBNAME "luaport"
#define LUAP_BUFFER 512
#define LUAP_CALL 0
#define LUAP_CAST 1
#define LUAP_TYPE_KEY "___type"
#define LUAP_MAP 0
#define LUAP_TUPLE 1
#define LUAP_LIST 2
#define LUAP_TUPLELIST 3
#define LUAP_TATOM "atom"

#define EXIT_BAD_MAIN 199
#define EXIT_BAD_VERSION 200
#define EXIT_BAD_TUPLE 201
#define EXIT_BAD_ATOM 202
#define EXIT_BAD_ARGS 203
#define EXIT_BAD_FUNC 204
#define EXIT_BAD_STRING 205
#define EXIT_WRONG_ARITY 206
#define EXIT_WRONG_STATUS 207
#define EXIT_FAIL_READ 208
#define EXIT_FAIL_WRITE 209
#define EXIT_FAIL_ALLOC 210

#define luap_checkatom(L, index) (char *)luaL_checkudata(L, index, LUAP_TATOM)
#define write_error(...) write_message("error", __VA_ARGS__)
#define write_info(...) write_message("info", __VA_ARGS__)

static void write_message(const char *type, const char* fmt, ...);

static inline size_t read4(char* buf)
{
	return buf[0] << 24 | buf[1] << 16 | buf[2] << 8 | buf[3];
}

static inline uint32_t swap4(uint32_t val)
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

static int read_term(char **buf, size_t *size)
{
	if (read_bytes(*buf, 4) != 4)
	{
		return -1;
	}
	
	size_t len = read4(*buf);

	if (len > *size)
	{
		*buf = (char*)realloc(*buf, len);
		*size = len;
	}

	return read_bytes(*buf, len);
}

static int write_term(ei_x_buff *eb)
{
	uint32_t len = swap4(eb->index);
	write_bytes((char*)&len, 4);
	len = write_bytes(eb->buff, eb->index);
	eb->index = 0;
	return len;
}

static void write_format(const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	ei_x_buff eb;
	ei_x_new(&eb);
	ei_x_format(&eb, fmt, args);

	write_term(&eb);
	ei_x_free(&eb);
	va_end(args);
}

static void write_message(const char *type, const char* fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	int len = vsnprintf(NULL, 0, fmt, ap) + 1;
	va_end(ap);

	char msg[len];

	va_start(ap, fmt);
	vsnprintf(msg, len, fmt, ap);
	va_end(ap);

	ei_x_buff eb;
	ei_x_new(&eb);
	ei_x_format(&eb, "{~a,~s}", type, msg);

	write_term(&eb);
	ei_x_free(&eb);
}

static void luap_tabletype(lua_State *L, int index, int type)
{
	if (type == LUAP_MAP)
	{
		if (lua_getmetatable(L, index))
		{
			lua_pushfstring(L, LUAP_TYPE_KEY);
			lua_pushnil(L);
			lua_rawset(L, -3);
			lua_pushnil(L);

			if (!lua_next(L, -2))
			{
				lua_pushnil(L);
				lua_setmetatable(L, index);
			}
			else
			{
				lua_pop(L, 2);
			}

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

		lua_pushfstring(L, LUAP_TYPE_KEY);
		lua_pushinteger(L, type);
		lua_rawset(L, -3);
		lua_setmetatable(L, index);
	}
}

static int luap_tostring(lua_State *L)
{
	char *atom = luap_checkatom(L, 1);
	lua_pushstring(L, atom);
	return 1;
}

static int e2l_any(const char *buf, int *index, lua_State *L);

static int e2l_long(const char *buf, int *index, lua_State *L)
{
	long l;

	if (ei_decode_long(buf, index, &l))
	{
		return 1;
	}

	lua_pushinteger(L, l);
	return 0;
}

static int e2l_float(const char *buf, int *index, lua_State *L)
{
	double d;

	if (ei_decode_double(buf, index, &d))
	{
		return 1;
	}

	lua_pushnumber(L, d);
	return 0;
}

static int e2l_string(const char *buf, int *index, lua_State *L)
{
	int type;
	int size;

	ei_get_type(buf, index, &type, &size);
	char *str = (char*)malloc(size + 1);

	if (ei_decode_string(buf, index, str))
	{
		free(str);
		return 1;
	}

	lua_createtable(L, size, 0);
	luap_tabletype(L, -1, LUAP_LIST);

	for (int i = 0; i < size; i++)
	{
		lua_pushinteger(L, str[i]);
		lua_rawseti(L, -2, i + 1);
	}

	free(str);
	return 0;
}

static int e2l_binary(const char *buf, int *index, lua_State *L)
{
	int type;
	long size = 0;

	ei_get_type(buf, index, &type, (int *)&size);
	char *str = (char*)malloc(size);

	if (ei_decode_binary(buf, index, str, &size))
	{
		free(str);
		return 1;
	}

	lua_pushlstring(L, str, size);
	free(str);
	return 0;
}

static int e2l_atom(const char *buf, int *index, lua_State *L)
{
	char *atom = (char *)lua_newuserdata(L, MAXATOMLEN);

	if (ei_decode_atom(buf, index, atom))
	{
		lua_pop(L, 1);
		return 1;
	}

	if (!strcmp(atom, "true"))
	{
		lua_pop(L, 1);
		lua_pushboolean(L, 1);
	}
	else if (!strcmp(atom, "false"))
	{
		lua_pop(L, 1);
		lua_pushboolean(L, 0);
	}
	/*else if (!strcmp(atom, "undefined"))
	{
		lua_pop(L, 1);
		lua_pushnil(L);
	}*/
	else
	{
		luaL_getmetatable(L, LUAP_TATOM);
		lua_setmetatable(L, -2);
	}

	return 0;
}

static int e2l_tuple(const char *buf, int *index, lua_State *L)
{
	int arity;

	if (ei_decode_tuple_header(buf, index, &arity))
	{
		return 1;
	}

	lua_createtable(L, arity, 0);
	luap_tabletype(L, -1, LUAP_TUPLE);

	for (int i = 1; i <= arity; i++)
	{
		e2l_any(buf, index, L);
		lua_rawseti(L, -2, i);
	}

	return 0;
}

static int e2l_list(const char *buf, int *index, lua_State *L)
{
	int arity;

	if (ei_decode_list_header(buf, index, &arity))
	{
		return 1;
	}

	lua_createtable(L, arity, 0);
	luap_tabletype(L, -1, LUAP_LIST);

	for (int i = 1; i <= arity; i++)
	{
		e2l_any(buf, index, L);
		lua_rawseti(L, -2, i);
	}
	
	ei_skip_term(buf, index);
	return 0;
}

static int e2l_emptylist(const char *buf, int *index, lua_State *L)
{
	lua_createtable(L, 0, 0);
	luap_tabletype(L, -1, LUAP_LIST);
	ei_skip_term(buf, index);
	return 0;
}

static int e2l_map(const char *buf, int *index, lua_State *L)
{
	int arity;

	if (ei_decode_map_header(buf, index, &arity))
	{
		return 1;
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

static int e2l_args(const char *buf, int *index, lua_State *L, int *nargs)
{
	int type;
	ei_get_type(buf, index, &type, nargs);

	if (type == ERL_LIST_EXT)
	{
		if (ei_decode_list_header(buf, index, nargs))
		{
			return 1;
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
		char *str = (char*)malloc(*nargs + 1);

		if (ei_decode_string(buf, index, str))
		{
			free(str);
			return 2;
		}
		
		for (int i = 0; i < *nargs; i++)
		{
			lua_pushinteger(L, str[i]);
		}

		free(str);
		return 0;
	}
	else if (type == ERL_NIL_EXT)
	{
		ei_skip_term(buf, index);
		return 0;
	}

	return 1;
}

static int e2l_any(const char *buf, int *index, lua_State *L)
{
	int type;
	int size;
	ei_get_type(buf, index, &type, &size);
	
	switch (type)
	{
		case ERL_SMALL_INTEGER_EXT:
		case ERL_INTEGER_EXT:
			return e2l_long(buf, index, L);
		case ERL_FLOAT_EXT:
			return e2l_float(buf, index, L);
		case ERL_STRING_EXT:
			return e2l_string(buf, index, L);
		case ERL_BINARY_EXT:
			return e2l_binary(buf, index, L);
		case ERL_ATOM_EXT:
			return e2l_atom(buf, index, L);
		case ERL_LIST_EXT:
			return e2l_list(buf, index, L);
		case ERL_NIL_EXT:
			return e2l_emptylist(buf, index, L);
		case ERL_SMALL_TUPLE_EXT:
		//case ERL_LARGE_TUPLE_EXT:
			return e2l_tuple(buf, index, L);
		case ERL_MAP_EXT:
			return e2l_map(buf, index, L);
		default:
			return 1;
	}

	return 0;
}

static void l2e_any(lua_State *L, int index, ei_x_buff *eb);

static void l2e_integer(lua_State *L, int index, ei_x_buff *eb)
{
	lua_Integer integer = lua_tointeger(L, index);
	ei_x_encode_longlong(eb, integer);
}

static void l2e_float(lua_State *L, int index, ei_x_buff *eb)
{
	lua_Number number = lua_tonumber(L, index);
	ei_x_encode_double(eb, number);
}

static void l2e_string_binary(lua_State *L, int index, ei_x_buff *eb)
{
	size_t len;
	const char *str = lua_tolstring(L, index, &len);
	ei_x_encode_binary(eb, str, len);
}

static void l2e_string_atom(lua_State *L, int index, ei_x_buff *eb)
{
	const char *str = lua_tostring(L, index);
	ei_x_encode_atom(eb, str);
}

static void l2e_boolean(lua_State *L, int index, ei_x_buff *eb)
{
	int boolean = lua_toboolean(L, index);
	ei_x_encode_boolean(eb, boolean);
}

static void l2e_table_map(lua_State *L, int index, ei_x_buff *eb)
{
	ei_x_buff eb_tmp;
	int nelems = 0;

	index = lua_absindex(L, index);
	ei_x_new(&eb_tmp);
	lua_pushnil(L);

	while (lua_next(L, index))
	{
		l2e_any(L, -2, &eb_tmp);
		l2e_any(L, -1, &eb_tmp);
		lua_pop(L, 1);
		nelems++;
	}

	ei_x_encode_map_header(eb, nelems);
	ei_x_append(eb, &eb_tmp);
	ei_x_free(&eb_tmp);
}

static void l2e_table_tuple(lua_State *L, int index, ei_x_buff *eb)
{
	index = lua_absindex(L, index);
	lua_Integer len = luaL_len(L, index);
	ei_x_encode_tuple_header(eb, len);

	for (int i = 1; i <= len; i++)
	{
		lua_rawgeti(L, index, i);
		l2e_any(L, -1, eb);
		lua_pop(L, 1);
	}
}

static void l2e_table_list(lua_State *L, int index, ei_x_buff *eb)
{
	index = lua_absindex(L, index);
	lua_Integer len = luaL_len(L, index);

	if (len > 0)
	{
		ei_x_encode_list_header(eb, len);

		for (int i = 1; i <= len; i++)
		{
			lua_rawgeti(L, index, i);
			l2e_any(L, -1, eb);
			lua_pop(L, 1);
		}
	}

	ei_x_encode_empty_list(eb);
}

static void l2e_table_tuplelist(lua_State *L, int index, ei_x_buff *eb)
{
	index = lua_absindex(L, index);
	lua_pushnil(L);

	while (lua_next(L, index))
	{
		ei_x_encode_list_header(eb, 1);
		ei_x_encode_tuple_header(eb, 2);
		l2e_any(L, -2, eb);
		l2e_any(L, -1, eb);
		lua_pop(L, 1);
	}

	ei_x_encode_empty_list(eb);
}

static void l2e_table(lua_State *L, int index, ei_x_buff *eb)
{
	if (luaL_getmetafield(L, index, LUAP_TYPE_KEY) != LUA_TNIL)
	{
		lua_Integer type = lua_tointeger(L, -1);
		lua_pop(L, 1);

		if (type == LUAP_TUPLE)
		{
			l2e_table_tuple(L, index, eb);
		}
		else if (type == LUAP_LIST)
		{
			l2e_table_list(L, index, eb);
		}
		else if (type == LUAP_TUPLELIST)
		{
			l2e_table_tuplelist(L, index, eb);
		}
	}
	else
	{
		l2e_table_map(L, index, eb);
	}
}

static void l2e_range_list(lua_State *L, int from, int to, ei_x_buff *eb)
{
	int count = to - from;

	if (count > 0)
	{
		ei_x_encode_list_header(eb, count);

		for (int i = from; i < to; i++)
		{
			l2e_any(L, i, eb);
		}
	}

	ei_x_encode_empty_list(eb);
}

static void l2e_userdata_atom(lua_State *L, int index, ei_x_buff *eb)
{
	char *atom = luap_checkatom(L, index);
	ei_x_encode_atom(eb, atom);
}

static void l2e_any(lua_State *L, int index, ei_x_buff *eb)
{
	int type = lua_type(L, index);

	switch (type)
	{
		case LUA_TNUMBER:
			if (lua_isinteger(L, index))
			{
				l2e_integer(L, index, eb);
			}
			else
			{
				l2e_float(L, index, eb);
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
		/*case LUA_TNIL:
			ei_x_encode_atom(eb, "undefined");
			break;*/
		case LUA_TUSERDATA:
			l2e_userdata_atom(L, index, eb);
			break;
		/*default:
			ei_x_encode_atom(eb, "unsupported");*/
	}
}

static int luaport_call_cont(lua_State *L, int status, long int ctx)
{
	return lua_gettop(L);
}

static int luaport_call(lua_State *L)
{
	luaL_checktype(L, 1, LUA_TSTRING);
	lua_pushinteger(L, LUAP_CALL);
	return lua_yieldk(L, lua_gettop(L), 0, luaport_call_cont);
}

static int luaport_cast_cont(lua_State *L, int status, long int ctx)
{
	return 0;
}

static int luaport_cast(lua_State *L)
{
	luaL_checktype(L, 1, LUA_TSTRING);
	lua_pushinteger(L, LUAP_CAST);
	return lua_yieldk(L, lua_gettop(L), 0, luaport_cast_cont);
}

static int luaport_info(lua_State *L)
{
	int i, top = lua_gettop(L);

	for (i = 1; i <= top; i++)
	{
		luaL_checkany(L, i);
	}

	ei_x_buff eb;
	ei_x_new_with_version(&eb);
	ei_x_encode_tuple_header(&eb, 2);
	ei_x_encode_atom(&eb, "info");
	ei_x_encode_list_header(&eb, top);

	for (i = 1; i <= top; i++)
	{
		l2e_any(L, i, &eb);
	}

	ei_x_encode_empty_list(&eb);
	write_term(&eb);
	ei_x_free(&eb);
	return 0;
}

static int luaport_asmap(lua_State *L)
{
	luaL_checktype(L, 1, LUA_TTABLE);
	luap_tabletype(L, 1, LUAP_MAP);
	return 1;
}

static int luaport_astuple(lua_State *L)
{
	luaL_checktype(L, 1, LUA_TTABLE);
	luap_tabletype(L, 1, LUAP_TUPLE);
	return 1;
}

static int luaport_aslist(lua_State *L)
{
	luaL_checktype(L, 1, LUA_TTABLE);
	luap_tabletype(L, 1, LUAP_LIST);
	return 1;
}

static int luaport_astuplelist(lua_State *L)
{
	luaL_checktype(L, 1, LUA_TTABLE);
	luap_tabletype(L, 1, LUAP_TUPLELIST);
	return 1;
}

static int luaport_toatom(lua_State *L)
{
	size_t len;
	const char *str = luaL_checklstring(L, 1, &len);
	char *atom = (char *)lua_newuserdata(L, MAXATOMLEN);
	memcpy(atom, str, len);
	luaL_getmetatable(L, LUAP_TATOM);
	lua_setmetatable(L, -2);
	return 1;
}

static const struct luaL_Reg luaport_func[] = {
	{"call", luaport_call},
	{"cast", luaport_cast},
	{"info", luaport_info},
	{"asmap", luaport_asmap},
	{"astuple", luaport_astuple},
	{"aslist", luaport_aslist},
	{"astuplelist", luaport_astuplelist},
	{"toatom", luaport_toatom},
	{NULL, NULL}
};

static const struct luaL_Reg atom_meta[] = {
	{"__tostring", luap_tostring},
	{NULL, NULL}
};

static int luaopen_luaport(lua_State *L)
{
	luaL_newmetatable(L, LUAP_TATOM);
	luaL_setfuncs(L, atom_meta, 0);
	lua_pushcfunction(L, luaport_info);
	lua_setglobal(L, "print");
	lua_newtable(L);
	lua_setglobal(L, "state");
	luaL_newlib(L, luaport_func);
	return 1;
}

int main(int argc, char *argv[])
{
	size_t buf_size = LUAP_BUFFER;
	char *buf = (char*)malloc(buf_size);

	if (buf == NULL)
	{
		exit(EXIT_FAIL_ALLOC);
	}

	int index = 0;
	int version;
	int arity;
	char func[MAXATOMLEN];
	int nargs;
	int status;
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
	luaL_requiref(L, LUAP_PORTLIBNAME, luaopen_luaport, 1);
	lua_settop(L, 0);

	if (luaL_dofile(L, "main.lua"))
	{
		exit(EXIT_BAD_MAIN);
	}

	while (read_term(&buf, &buf_size) > 0)
	{
		index = 0;

		if (ei_decode_version(buf, &index, &version))
		{
			exit(EXIT_BAD_VERSION);
		}

		if (ei_decode_tuple_header(buf, &index, &arity))
		{
			exit(EXIT_BAD_TUPLE);
		}

		if (arity < 1 || arity > 2)
		{
			exit(EXIT_WRONG_ARITY);
		}

		if (arity == 2)//call
		{
			if (ei_decode_atom(buf, &index, func))
			{
				exit(EXIT_BAD_ATOM);
			}

			if (lua_getglobal(L, func) != LUA_TFUNCTION)
			{
				exit(EXIT_BAD_FUNC);
			}
		}

		if (e2l_args(buf, &index, L, &nargs))
		{
			exit(EXIT_BAD_ARGS);
		}

		resume:
		status = lua_resume(L, NULL, nargs);

		if (status > LUA_YIELD)
		{
			ei_x_encode_version(&eb);
			ei_x_encode_tuple_header(&eb, 2);
			ei_x_encode_atom(&eb, "error");
			l2e_string_binary(L, -1, &eb);

			write_term(&eb);
			L = lua_newthread(L);
		}
		else if (status == LUA_OK)
		{
			ei_x_encode_version(&eb);
			ei_x_encode_tuple_header(&eb, 2);
			ei_x_encode_atom(&eb, "ok");

			int nresults = lua_gettop(L);

			if (nresults > 0)
			{
				ei_x_encode_list_header(&eb, nresults);

				for (int index = 1; index <= nresults; index++)
				{
					l2e_any(L, index, &eb);
				}

				lua_settop(L, 0);
			}

			ei_x_encode_empty_list(&eb);
			write_term(&eb);
		}
		else if (status == LUA_YIELD)
		{
			int yield = lua_tointeger(L, -1);

			if (yield == LUAP_CALL)
			{
				ei_x_encode_version(&eb);
				ei_x_encode_tuple_header(&eb, 2);
				ei_x_encode_atom(&eb, "call");
				ei_x_encode_tuple_header(&eb, 2);

				l2e_string_atom(L, 1, &eb);
				l2e_range_list(L, 2, lua_gettop(L), &eb);
				lua_settop(L, 0);

				write_term(&eb);
			}
			else
			{
				if (yield == LUAP_CAST)
				{
					ei_x_encode_version(&eb);
					ei_x_encode_tuple_header(&eb, 2);
					ei_x_encode_atom(&eb, "cast");
					ei_x_encode_tuple_header(&eb, 2);

					l2e_string_atom(L, 1, &eb);
					l2e_range_list(L, 2, lua_gettop(L), &eb);
				}
				else
				{
					int nresults = lua_gettop(L);

					ei_x_encode_version(&eb);
					ei_x_encode_tuple_header(&eb, 2);
					ei_x_encode_atom(&eb, "info");
					ei_x_encode_list_header(&eb, nresults);

					for (int i = 1; i < nresults; i++)
					{
						l2e_any(L, i, &eb);
					}

					ei_x_encode_empty_list(&eb);
				}

				write_term(&eb);
				lua_settop(L, 0);
				nargs = 0;
				goto resume;
			}
		}
		else
		{
			exit(EXIT_WRONG_STATUS);
		}
	}

	free(buf);
	ei_x_free(&eb);

	if (L != NULL)
	{
		lua_close(L);
	}

	return 0;
}
