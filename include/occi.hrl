%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-type(uri() :: binary()).

%%% OCCI Category ID
-record(occi_cid, {scheme    = undefined :: atom(),
			   term      = undefined :: atom()}).
-type(occi_cid() :: #occi_cid{}).

%%% OCCI Type (Category or derivative)
-record(occi_type, {id         = #occi_cid{} :: occi_cid(),
		    module     = undefined           :: atom(),
		    backend    = undefined           :: atom()}).
-type(occi_type() :: #occi_type{}).

%%% OCCI Entity
-type(occi_entity_id() :: binary()).

-record(occi_entity, {id         = undefined :: occi_entity_id() | undefined,
		      module                 :: atom(),
		      title      = <<>>      :: binary(),
		      attributes             :: [{atom(), any()}]}).
-type(occi_entity() :: #occi_entity{}).

%%% OCCI Filter
-type(occi_filter() :: any()).
