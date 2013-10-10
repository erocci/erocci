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
			 mixins         :: [occi_cid()]}).
-type(occi_extension() :: #occi_extension{}).

%%% OCCI Attribute description
-record(occi_attr_spec, {id             :: atom(),
			 type           :: {atom(), mfa()} | atom(),
			 properties     :: [term()]}).
-type(occi_attr_spec() :: #occi_attr_spec{}).

%%% OCCI Category ID
-record(occi_cid, {scheme    = undefined :: atom() | uri(),
		   term      = undefined :: atom(),
		   class                 :: occi_class()}).
-type(occi_cid() :: #occi_cid{}).

%%% OCCI Kind
-record(occi_kind, {id         = #occi_cid{}  :: occi_cid(),
		    title      = undefined    :: binary(),
		    attributes = []           :: [occi_attr_spec()],
		    rel        = []           :: occi_cid(),
		    actions    = []           :: [occi_action_spec()],
		    location                  :: uri()}).
-type(occi_kind() :: #occi_kind{}).
%%% OCCI Mixin

-record(occi_mixin, {id         = #occi_cid{}  :: occi_cid(),
		     title      = undefined    :: binary(),
		     depends    = []           :: [occi_cid()],
		     applies    = []           :: [occi_cid()],
		     attributes = []           :: [occi_attr_spec()],
		     actions    = []           :: [occi_action_spec()],
		     location                  :: uri()}).
-type(occi_mixin() :: #occi_mixin{}).

%%% OCCI Action Spec
-record(occi_action_spec, {id         = #occi_cid{}  :: occi_cid(),
			   title      = undefined    :: binary(),
			   attributes = []           :: [occi_attr_spec()]}).
-type(occi_action_spec() :: #occi_action_spec{}).

%%% OCCI Category
-type(occi_category() :: occi_kind() | occi_mixin() | occi_action()).

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

%%% OCCI Action
-record(occi_action, {id         = #occi_cid{}  :: occi_cid(),
		      attributes = []           :: [{atom(), any()}]}).
-type(occi_action() :: #occi_action{}).

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
