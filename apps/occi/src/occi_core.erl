%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_core).

-include("occi.hrl").

%% API
-export([create_kind/6]).

%%%===================================================================
%%% API
%%%===================================================================
create_kind(Scheme, Term, Title, Related, Location, Attrs) ->
		Cat = #occi_category{scheme=Scheme, 
												 term=Term, 
												 title=Title, 
												 location=Location, 
												 attrs=Attrs},
		#occi_kind{super=Cat, 
							 rel=Related}.
