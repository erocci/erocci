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
    Mapping = [
	       {#occi_cid{scheme=?SCHEME_INFRA, term='compute', class=kind}, "/compute/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='storage', class=kind}, "/storage/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='storagelink', class=kind}, "/storagelink/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='network', class=kind}, "/network/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='networkinterface', class=kind}, "/networkinterface/"},
	       {#occi_cid{scheme=?SCHEME_NET, term='ipnetwork', class=mixin}, "/ipnetwork/"},
	       {#occi_cid{scheme=?SCHEME_NET_IF, term='ipnetworkinterface', class=mixin}, "/ipnetworkinterface/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='os_tpl', class=mixin}, "/os_tpl/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='resource_tpl', class=mixin}, "/resource_tpl/"}
	      ],
    Extensions = {extensions,
		  [{xml, "schemas/occi-infrastructure.xml"}], 
		  Mapping},
    Backends = {backends, 
		[{mnesia, occi_backend_mnesia, [], "/"},
		 {dummy1, occi_backend_dummy, [], "/dummy1"},
		 {dummy2, occi_backend_dummy, [], "/dummy1/with/a/long/path"},
		 {dummy3, occi_backend_dummy, [], "/dummy2"}]},
    Listeners = {listeners, 
		 [{http, occi_http, [{port, 8080}]}]},
    Handlers = {handlers, 
		[{pid, self()}]},
    occi:config([{name, "http://localhost:8080"},
		 Extensions, 
		 Backends,
		 Listeners,
		 Handlers]),
    register(?MODULE, self()),
    loop().

stop() ->
    application:stop(occi),
    ?MODULE ! stop.

loop() ->
    receive
	stop ->
	    exit(the_end);
	{From, #occi_action{id=ActionId}, #occi_node{id=Id}} ->
	    lager:info("Apply action: ~p(~p)~n", [lager:pr(ActionId, ?MODULE),
						  lager:pr(Id, ?MODULE)]),
	    From ! false,
	    loop();
	_ ->
	    loop()
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
