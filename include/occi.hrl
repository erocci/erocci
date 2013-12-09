%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-type(uri() :: [binary()]).
-type(occi_class() :: kind | mixin | action).

%%% OCCI Extension
-record(occi_extension, {name           :: atom(),
			 scheme         :: atom(),
			 version        :: term(),
			 kinds          :: [occi_cid()],
			 mixins         :: [occi_cid()],
			 types          :: term()           % dict
			}).
-type(occi_extension() :: #occi_extension{}).

%%% OCCI Category ID
-record(occi_cid, {scheme    = undefined :: atom() | uri(),
		   term      = undefined :: atom(),
		   class                 :: occi_class()}).
-type(occi_cid() :: #occi_cid{}).

-record(occi_category, {ref      :: reference(),
			id       :: #occi_cid{},
			location :: uri()}).
-type(occi_category() :: #occi_category{}).

-record(occi_action, {ref      :: reference(),
		      id       :: #occi_cid{},
		      location :: uri()}).
-type(occi_action() :: #occi_action{}).

%%% OCCI simple types
-record(occi_type, {id           :: occi_type_id(),
		    f            :: fun()}).
-type(occi_type_id() :: atom() | {atom(), atom()}).
-type(occi_type() :: #occi_type{}).

%%% OCCI Attribute description
-record(occi_attr, {id                           :: atom(),
		    type_id                      :: occi_type_id(),
		    check                        :: fun(),
		    scalar          = true       :: boolean(),
		    properties                   :: [term()],
		    value           = undefined  :: any()}).
-type(occi_attr() :: #occi_attr{}).

%%% OCCI Resource
-record(occi_resource, {id         = undefined :: uri(),
			cid                    :: occi_cid(),
			title      = undefined :: binary(),
		        summary    = undefined :: binary(),
			attributes = []        :: [{atom(), any()}],
			links      = []        :: [uri()],
			mixins     = []        :: [occi_cid()]}).
-type(occi_resource() :: #occi_resource{}).

%%% OCCI Link
-record(occi_link, {id         = undefined :: uri(),
		    cid                    :: occi_cid(),
		    title      = undefined :: binary(),
		    attributes = []        :: [{atom(), any()}],
		    source                 :: uri(),
		    target                 :: uri(),
		    mixins     = []        :: [occi_cid()]}).
-type(occi_link() :: #occi_link{}).

%%% OCCI Entity
-type(occi_entity() :: #occi_resource{} | #occi_link{}).

%%% OCCI Filter
-type(occi_filter() :: any()).

%%% OCCI hooks
-record(hook_rec, {key    :: {hook_name(), #occi_cid{}},
		   ref    :: reference()}).
-type(hook_rec() :: #hook_rec{}).

-type(hook() :: {hook_name(), hook_fun()}).
-type(hook_name() :: on_save | on_update | on_delete | on_action).
-type(hook_fun() :: {atom(), atom()} | fun()).
