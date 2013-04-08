%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean@bison.home>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean@bison.home>
%%%-------------------------------------------------------------------
-module(occi_infra).

-include("occi.hrl").

%% API
-export([create_compute_kind/0, 
				 create_network_kind/0, 
				 create_storage_kind/0]).

%%%===================================================================
%%% API
%%%===================================================================
create_compute_kind() ->
		occi_core:create_kind([ ?OCCI_BASE_SCHEME, <<"infrastructure#">> ],
													compute, 
													<<"Compute Resource">>,
													[ ?OCCI_BASE_SCHEME, <<"core#resource">> ],
													<<"/compute/">>,
													[#occi_attr{key='occi.compute.architecture'},
													 #occi_attr{key='occi.compute.cores'},
													 #occi_attr{key='occi.compute.hostname'},
													 #occi_attr{key='occi.compute.speed'},
													 #occi_attr{key='occi.compute.memory'},
													 #occi_attr{key='occi.compute.state'}
													]).

create_network_kind() ->
		occi_core:create_kind([ ?OCCI_BASE_SCHEME, <<"infrastructure#">> ],
													network, 
													"Network Resource",
													[ ?OCCI_BASE_SCHEME, <<"core#resource">> ],
													<<"/network/">>,
													[#occi_attr{key='occi.network.vlan'},
													 #occi_attr{key='occi.network.label'},
													 #occi_attr{key='occi.network.state'}
													]).

create_storage_kind() ->
		occi_core:create_kind([ ?OCCI_BASE_SCHEME, <<"infrastructure#">> ],
													storage, 
													"Storage Resource", 
													[ ?OCCI_BASE_SCHEME, <<"core#resource">> ],
													<<"/storage/">>,
													[#occi_attr{key='occi.storage.size'},
													 #occi_attr{key='occi.storage.state'}
													]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
