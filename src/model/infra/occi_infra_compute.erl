%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 12 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_infra_compute).

-occi_scheme('http://schemas.ogf.org/occi/infrastructure#').
-occi_term(compute).
-occi_title(<<"Compute resource">>).
-occi_class(kind).
-occi_entity_type("Compute").
-occi_relation({'http://schemas.ogf.org/occi/core#', resource}).

%%%
%%% Attributes
%%% {Name :: atom(), Properties :: [occi_attr_property()], Checker :: mfa()}
%%%
-occi_attribute({'occi.compute.architecture', [], {occi_types, is_enum, [x86, x64]}}).
-occi_attribute({'occi.compute.cores', [], {occi_types, is_integer, []}}).
-occi_attribute({'occi.compute.hostname', [], {occi_types, is_alnum, []}}).
-occi_attribute({'occi.compute.speed', [], {occi_types, is_float, []}}).
-occi_attribute({'occi.compute.memory', [], {occi_types, is_integer, []}}).
-occi_attribute({'occi.compute.state', [required, immutable], {occi_types, is_enum, [active, inactive, suspend]}}).

%%%
%%% Actions
%%% {Name  :: atom(), Title :: binary(), Attributes :: [{occi_attr_key(), mfa()}]}
%%%
-occi_action({start, <<"Start Compute Resource">>, []}).
-occi_action({stop, <<"Stop Compute Resource">>, [{method, {occi_types, is_enum, [[graceful, acpioff, poweroff]]}}]}).
-occi_action({restart, <<"Restart Compute Resource">>, [{method, {occi_types, is_enum, [[graceful, warm, cold]]}}]}).
-occi_action({suspend, <<"Suspend Compute Resource">>, [{method, {occi_types, is_enum, [[hibernate, suspend]]}}]}).

%%%
%%% Implementation
%%%
-export([start/1,
	 stop/2,
	 restart/2,
	 suspend/2]).

start(State) ->
    {ok, State}.

stop(State, _Method) ->
    {ok, State}.

restart(State, _Method) ->
    {ok, State}.

suspend(State, _Method) ->
    {ok, State}.
