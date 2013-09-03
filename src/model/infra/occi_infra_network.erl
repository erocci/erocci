%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 12 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_infra_network).

-occi_scheme('http://schemas.ogf.org/occi/infrastructure#').
-occi_term(network).
-occi_title(<<"Network resource">>).
-occi_class(kind).
-occi_entity_type(link).
-occi_relation({'http://schemas.ogf.org/occi/core#', link}).

%%%
%%% Attributes
%%% {Name :: atom(), Properties :: [occi_attr_property()], Checker :: mfa()}
%%%
-occi_attribute({'occi.network.vlan', [], {occi_types, is_range, [0, 4095]}}).
-occi_attribute({'occi.network.label', [], {occi_types, is_alnum, []}}).
-occi_attribute({'occi.network.state', [required, immutable], {occi_types, is_enum, [active, inactive, suspend]}}).

%%%
%%% Actions
%%% {Name  :: atom(), Title :: binary(), Attributes :: [{occi_attr_key(), mfa()}]}
%%%
-occi_action({up, <<"Put in active state">>, []}).
-occi_action({down, <<"Put in inactive state">>, []}).

%%%
%%% Implementation
%%%
-export([up/1, down/1]).

up(State) ->
    {ok, State}.

down(State) ->
    {ok, State}.
