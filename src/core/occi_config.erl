%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_config).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([start/0,
	 load/1,
	 get/2,
	 set/2,
	 to_url/1,
	 gen_id/1]).

-define(TABLE, ?MODULE).

start() ->
    lager:info("Starting OCCI config manager"),
    ?TABLE = ets:new(?TABLE, [set, public, {keypos, 1}, named_table]),
    ok.

%%% @doc Config is a proplist, which can be overriden by application env
-spec load(list()) -> ok.
load(Config) ->
    Env = application:get_all_env(occi),
    setup(Env ++ Config).

get(Name, Default) ->
    case ets:match_object(?TABLE, {Name, '_'}) of
	[] ->
	    Default;
	[{_, Value}] ->
	    Value
    end.

set(Name, Value) ->
    ets:insert(?TABLE, {Name, Value}).

to_url(#uri{}=Uri) ->
    case get(name, undefined) of
	undefined ->
	    throw({error, undefined_name});
	#uri{scheme=Scheme, host=Host, port=Port, userinfo=UserInfo} ->
	    Uri#uri{scheme=Scheme, host=Host, port=Port, userinfo=UserInfo}
    end.

-spec gen_id(string() | binary()) -> uri().
gen_id(Prefix) when is_binary(Prefix) ->
    gen_id(binary_to_list(Prefix));
gen_id(Prefix) when is_list(Prefix) ->
    #uri{host=Host}=Server = get(name, undefined),
    Id = uuid:to_string(uuid:uuid3(uuid:uuid4(), Host)),
    Server#uri{path=Prefix++Id}.

%%%
%%% Private
%%%
setup(Props) ->
    lager:debug("setup(~p)~n", [Props]),
    case proplists:get_value(extensions, Props) of
	undefined -> ok;
	{Ext, Map} -> load_extensions(Ext, Map)
    end,
    case proplists:get_value(backends, Props) of
	undefined -> ok;	    
	Backends -> load_backends(Backends)
    end,
    case proplists:get_value(listeners, Props) of
	undefined -> ok;
	Listeners -> load_listeners(Listeners)
    end,
    case proplists:get_value(name, Props) of
	undefined -> ok;
	Name ->
	    ets:insert(?TABLE, {name, occi_uri:parse(Name)})
    end,
    case proplists:get_value(handlers, Props) of
	undefined -> ok;
	Handlers -> load_handlers(Handlers)
    end.

load_extensions([], _) ->
    ok;
load_extensions([E|Extensions], Mapping) ->
    case occi_category_mgr:register_extension(E, Mapping) of
	ok ->
	    load_extensions(Extensions, Mapping);
	{error, Err} ->
	    throw({error, Err})
    end.

load_backends([]) ->
    ok;
load_backends([B|Backends]) ->
    case occi_store:register(B) of
	{ok, _Pid} ->
	    load_backends(Backends);
	{error, Err} ->
	    throw({error, Err})
    end.

load_listeners([]) ->
    ok;
load_listeners([L|Listeners]) ->
    case occi_listener:register(L) of
	{ok, _Pid} ->
	    load_listeners(Listeners);
	{error, Err} ->
	    {error, Err}
    end.

load_handlers([]) ->
    ok;
load_handlers([H|Handlers]) ->
    case occi_hook:register(H) of
	ok ->
	    load_handlers(Handlers);
	{error, Err} ->
	    {error, Err}
    end.
