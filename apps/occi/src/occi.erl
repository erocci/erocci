%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2013 Jean Parpaillon.

%% @doc occi startup code
-module(occi).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-behaviour(application).

-export([start/0, stop/0, db_init/0]).

% application callbacks
-export([start/2,stop/1]).

%% @spec start() -> ok
%% @doc Start the occi server.
start() ->
		ok = ensure_started(occi).

%% @spec stop() -> ok
%% @doc Stop the occi server.
stop() ->
		Res = application:stop(occi),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto),
		application:stop(exmpp),
		application:stop(inets),
		occi_config:stop(),
		Res.

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for occi.
start(_Type, _StartArgs) ->
		db_init(),
		occi_config:start(),
		ok = ensure_started(inets),
		ok = ensure_started(exmpp),
		ok = ensure_started(crypto),
		ok = ensure_started(ranch),
		ok = ensure_started(cowboy),
    occi_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for occi.
stop(_State) ->
		ok.

%% ===================================================================
%% Internal
%% ===================================================================
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

db_init() ->
    case mnesia:system_info(extra_db_nodes) of
	[] ->
	    mnesia:create_schema([node()]);
	_ ->
	    ok
    end,
    application:start(mnesia, permanent),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).
