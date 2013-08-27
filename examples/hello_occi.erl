%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(hello_occi).

-include_lib("occi.hrl").

-export([start/0, stop/1]).

-define(SCHEMA_CORE, 'http://schemas.ogf.org/occi/core').
-define(SCHEMA_INFRA, 'http://schemas.ogf.org/occi/infrastructure').
-define(RIAK_NODE, "127.0.0.1").
-define(RIAK_PORT, 8087).

start() ->
    {ok, Pid} = occi:start(),
    Store = occi_store:new(riak, occi_backend_riak, {?RIAK_NODE, ?RIAK_PORT}),
    CatId = #occi_category_id{scheme='http://schemas.ogf.org/occi/infrastructure', term=compute},
    Related = #occi_category_id{scheme='http://schemas.ogf.org/occi/core', term=resource},
    Attributes = [ #occi_attr{key='occi.compute.architecture', callback={occi_types, is_enum, [<<"x86">>, <<"x64">>]}},
		   #occi_attr{key='occi.compute.cores', callback={occi_types, is_integer}},
		   #occi_attr{key='occi.compute.hostname', callback={occi_types, is_alnum}},
		   #occi_attr{key='occi.compute.speed', callback={occi_types, is_float}},
		   #occi_attr{key='occi.compute.memory', callback={occi_types, is_float}},
		   #occi_attr{key='occi.compute.state', 
			      property=[ immutable, required ], 
			      callback={occi_types, is_enum, [<<"active">>, <<"inactive">>, <<"suspended">>]}}],
    Kind = occi_kind:new(CatId, <<"Compute Resource">>, Related, Attributes),
    occi_store:register(Store, Kind),
    Pid.

stop(Pid) ->
    occi:stop(Pid).
