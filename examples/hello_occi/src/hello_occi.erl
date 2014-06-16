%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(hello_occi).
-compile([{parse_transform, lager_transform}]).

-include_lib("occi.hrl").

-export([start/0, stop/0, loop/0]).
%% Hooks
-export([on_save/1, on_update/2, on_delete/1, on_action/2]).

start() ->
    application:start(occi),
    Schemas = {schemas, [{xml, "schemas/occi-infrastructure.xml"}]},
    Backends = {backends, 
		[{dummy1, occi_backend_dummy, [], "/"},
		 {mnesia, occi_backend_mnesia, [Schemas], "/store"},
		 {dummy2, occi_backend_dummy, [], "/dummy1/with/a/long/path"},
		 {dummy3, occi_backend_dummy, [], "/dummy2"}]},
    Handlers = {handlers, 
		[{pid, self()}]},
    occi:config([Backends, Handlers]),
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
