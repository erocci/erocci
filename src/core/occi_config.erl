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
	 to_url/1,
	 gen_id/1]).

-define(TABLE, ?MODULE).

start() ->
    lager:info("Starting OCCI config manager"),
    ?TABLE = ets:new(?TABLE, [set, public, {keypos, 1}, named_table]),
    ok.

load(Config) ->
    load1(Config, []).

get(Name, Default) ->
    case ets:match_object(?TABLE, {Name, '_'}) of
	[] ->
	    Default;
	[{_, Value}] ->
	    Value
    end.

to_url(#uri{}=Uri) ->
    Name = get(name, undefined),
    Uri#uri{scheme=Name#uri.scheme, host=Name#uri.host, port=Name#uri.port}.

-spec gen_id(string() | binary()) -> binary().
gen_id(Prefix) when is_binary(Prefix) ->
    gen_id(binary_to_list(Prefix));
gen_id(Prefix) when is_list(Prefix) ->
    #uri{host=Host}=Server = get(name, undefined),
    Id = uuid:to_string(uuid:uuid3(uuid:uuid4(), Host)),
    Server#uri{path=Prefix++Id}.

%%%
%%% Private
%%%

% First pass: some options must be parsed first
load1([], Acc) ->
    % 2nd pass
    load2(Acc);
load1([{name, Name}|Configs], Acc) ->
    load_name(Name),
    load1(Configs, Acc);
load1([H|T], Acc) ->
    load1(T, [H|Acc]).

% Second pass
load2([]) ->
    ok;
load2([{extensions, Extensions, Mapping}|Configs]) ->
    load_extensions(Extensions, Mapping),
    load2(Configs);
load2([{backends, Backends}|Configs]) ->
    load_backends(Backends),
    load2(Configs);
load2([{listeners, Listeners}|Configs]) ->
    load_listeners(Listeners),
    load2(Configs);
load2([{handlers, Handlers}|Configs]) ->
    load_handlers(Handlers),
    load2(Configs).

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

load_name(Name) ->
    ets:insert(?TABLE, {name, occi_uri:parse(Name)}).
