%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(hello_occi).
-compile([{parse_transform, lager_transform}]).

-include_lib("occi.hrl").

-define(BASE, <<"http://localhost:8080">>).

-export([start/0, stop/0, loop/0]).
%% Hooks
-export([on_save/1, on_update/2, on_delete/1, on_action/2]).

start() ->
    application:start(occi),
    occi:register_backend({riak, occi_backend_riak, 
			   [{ip, "127.0.0.1"}, {port, 8087}]},
			  <<"/">>), 
    Hooks = [
	     {on_save, {?MODULE, on_save}},
	     {on_update, {?MODULE, on_update}},
	     {on_delete, {?MODULE, on_delete}},
	     {on_action, {?MODULE, on_action}}
	    ],
    occi:register_category({mod, occi_infra_compute}, <<"/compute/">>, Hooks),
    occi:register_category({mod, occi_infra_network}, <<"/network/">>, Hooks),
    occi:register_category({mod, occi_infra_ipnetworking}, <<"/ipnetworking/">>, Hooks),
    occi:register_listener(occi_http, [{port, 8080}]),
    register(?MODULE, self()),
    erlang:hibernate(?MODULE, loop, []).

stop() ->
    application:stop(occi),
    ?MODULE ! stop.

loop() ->
    receive
	stop ->
	    exit(the_end);
	_ ->
	    erlang:hibernate(?MODULE, loop, [])
    end.

%%%
%%% Hooks
%%%
-spec on_save(occi_entity()) -> {reply, occi_entity()} | noreply.
on_save(Obj) ->
    {reply, Obj}.

-spec on_update(occi_entity(), occi_entity()) -> {reply, occi_entity()} | noreply.
on_update(_Old, New) ->
    {reply, New}.

-spec on_delete(occi_entity()) -> reply | noreply.
on_delete(Obj) ->
    {reply, Obj}.

-spec on_action(occi_entity(), occi_action()) -> {reply, occi_entity()} | reply.
on_action(Obj, _Action) ->
    {reply, Obj}.
