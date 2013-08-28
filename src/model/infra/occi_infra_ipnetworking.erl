%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 12 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_infra_ipnetworking).

-occi_scheme('http://schemas.ogf.org/occi/infrastructure/network#').
-occi_term(ipnetwork).
-occi_title(<<"IP Networking Mixin">>).
-occi_class(mixin).
-occi_entity_type("IP Networking").

%%%
%%% Attributes
%%% {Name :: atom(), Properties :: [occi_attr_property()], Checker :: mfa()}
%%%
-occi_attribute({'occi.network.address', [], {occi_types, is_cidr, []}}).
-occi_attribute({'occi.network.gateway', [], {occi_types, is_ipaddress, []}}).
-occi_attribute({'occi.network.allocation', [], {occi_types, is_enum, [[dynamic, static]]}}).

%%%
%%% Implementation
%%%
-export([up/1, down/1]).

up(State) ->
    {ok, State}.

down(State) ->
    {ok, State}.
