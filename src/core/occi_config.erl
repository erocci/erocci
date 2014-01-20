%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_config).
-compile([{parse_transform, lager_transform}]).

-export([start/0,
	 load/1,
	 get/2]).

-define(TABLE, ?MODULE).

start() ->
    lager:info("Starting OCCI config manager"),
    ?TABLE = ets:new(?TABLE, [set, public, {keypos, 1}, named_table]),
    ok.

load([]) ->
    ok;
load([{extensions, Extensions, Mapping}|Configs]) ->
    load_extensions(Extensions, Mapping),
    load(Configs);
load([{backends, Backends}|Configs]) ->
    load_backends(Backends),
    load(Configs);
load([{listeners, Listeners}|Configs]) ->
    load_listeners(Listeners),
    load(Configs);
load([{name, Name}|Configs]) ->
    ets:insert(?TABLE, {name, Name}),
    load(Configs).

get(Name, Default) ->
    case ets:match_object(?TABLE, {Name, '_'}) of
	[] ->
	    Default;
	[{_, Value}] ->
	    Value
    end.

%%%
%%% Private
%%%
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

