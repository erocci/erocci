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
-define(SCHEME_INFRA, 'http://schemas.ogf.org/occi/infrastructure#').
-define(SCHEME_NET, 'http://schemas.ogf.org/occi/infrastructure/network#').
-define(SCHEME_NET_IF, 'http://schemas.ogf.org/occi/infrastructure/networkinterface#').

-export([start/0, stop/0, loop/0]).
%% Hooks
-export([on_save/1, on_update/2, on_delete/1, on_action/2]).

start() ->
    application:start(occi),
    Mapping = dict:from_list([
			      {#occi_cid{scheme=?SCHEME_INFRA, term='compute', class=kind}, 
			       {<<"/compute/">>, mnesia}},
			      {#occi_cid{scheme=?SCHEME_INFRA, term='storage', class=kind}, 
			       {<<"/storage/">>, mnesia}},
			      {#occi_cid{scheme=?SCHEME_INFRA, term='storagelink', class=kind}, 
			       {<<"/storagelink/">>, mnesia}},
			      {#occi_cid{scheme=?SCHEME_INFRA, term='network', class=kind},
			       {<<"/network/">>, mnesia}},
			      {#occi_cid{scheme=?SCHEME_INFRA, term='networkinterface', class=kind}, 
			       {<<"/networkinterface/">>, mnesia}},
			      {#occi_cid{scheme=?SCHEME_NET, term='ipnetwork', class=mixin},
			       {<<"/ipnetwork/">>, mnesia}},
			      {#occi_cid{scheme=?SCHEME_NET_IF, term='ipnetworkinterface', class=mixin}, 
			       {<<"/ipnetworkinterface/">>, mnesia}},
			      {#occi_cid{scheme=?SCHEME_INFRA, term='os_tpl', class=mixin},
			       {<<"/os_tpl/">>, mnesia}},
			      {#occi_cid{scheme=?SCHEME_INFRA, term='resource_tpl', class=mixin},
			       {<<"/resource_tpl/">>, mnesia}}
			     ]),
    occi_category_mgr:register_extension({xml, "schemas/occi-infrastructure.xml"}, Mapping),
    %%occi_store:register_backend({riak, occi_backend_riak, [{ip, "127.0.0.1"}, {port, 8087}]}),
    occi_store:register({mnesia, occi_backend_mnesia, []}),
    %% Hooks = [
    %% 	     {on_save, {?MODULE, on_save}},
    %% 	     {on_update, {?MODULE, on_update}},
    %% 	     {on_delete, {?MODULE, on_delete}},
    %% 	     {on_action, {?MODULE, on_action}}
    %% 	    ],
    %% occi:register_category({mod, occi_infra_compute}, <<"/compute/">>, Hooks),
    %% occi:register_category({mod, occi_infra_network}, <<"/network/">>, Hooks),
    %% occi:register_category({mod, occi_infra_ipnetworking}, <<"/ipnetworking/">>, Hooks),
    occi_listener:register(http, occi_http, [{port, 8080}]),
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

-spec on_action(occi_entity(), any()) -> {reply, occi_entity()} | reply.
on_action(Obj, _Action) ->
    {reply, Obj}.
