%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-type(occi_class() :: kind | mixin | action).

-define(HTTP_SERVER_ID, "erocci OCCI/1.1").

%%%% URI and al.
-record(uri, {scheme   = undefined    :: atom(),
	      userinfo = ""           :: string(),
	      host     = ""           :: string(),
	      port                    :: integer(),
	      path     = ""           :: string(),
	      query    = ""           :: string()}).
-type(uri() :: #uri{}).

%%% OCCI Extension
-record(occi_extension, {name           :: atom(),
			 scheme         :: atom(),
			 version        :: term(),
			 kinds          :: [occi_cid()],
			 mixins         :: [occi_cid()],
			 types          :: [occi_type()]
			}).
-type(occi_extension() :: #occi_extension{}).

-record(occi_collection, {cid                    :: occi_cid(),
			  entities  = undefined  :: set()}).    % ordset
-type(occi_collection() :: #occi_collection{}).

%%% OCCI Category ID
-record(occi_cid, {scheme    = undefined :: atom() | uri(),
		   term      = undefined :: atom(),
		   class     = undefined :: occi_class()}).
-type(occi_cid() :: #occi_cid{}).

-record(occi_kind, {id                  :: #occi_cid{},
		    title               :: binary(),
		    location            :: uri(),
		    parent              :: #occi_cid{},
		    attributes          :: dict(),            % orddict
		    actions     = []    :: [occi_action()]
		   }).
-type(occi_kind() :: #occi_kind{}).

-record(occi_mixin, {id                      :: #occi_cid{},
		     title       = undefined :: binary(),
		     location    = undefined :: uri(),
		     depends     = []        :: [#occi_cid{}],
		     applies     = []        :: [#occi_cid{}],
		     attributes              :: dict(),            % orddict
		     actions     = []        :: [occi_action()]
		    }).
-type(occi_mixin() :: #occi_mixin{}).

-type(occi_category() :: occi_kind() | occi_mixin()).

-record(occi_action, {id                        :: #occi_cid{},
		      title                     :: binary(),
		      location    = undefined   :: uri(),
		      attributes                :: dict()             % orddict
		     }).
-type(occi_action() :: #occi_action{}).

%%% OCCI simple types
-record(occi_type, {id           :: occi_type_id(),
		    f            :: fun()}).
-type(occi_type_id() :: atom() | {atom(), atom()}).
-type(occi_type() :: #occi_type{}).

%%% OCCI Attribute description
-type(occi_attr_key() :: atom()).
-record(occi_attr, {id                           :: occi_attr_key(),
		    type_id                      :: occi_type_id(),
		    title           = undefined  :: binary(),
		    scalar          = true       :: boolean(),
		    properties                   :: [term()],
		    value           = undefined  :: any()}).
-type(occi_attr() :: #occi_attr{}).

%%% OCCI Resource
-record(occi_resource, {id         = undefined :: uri(),
			cid        = undefined :: occi_cid(),
			title      = undefined :: binary(),
		        summary    = undefined :: binary(),
			attributes = undefined :: dict(),         % orddict()
			links                  :: set(),
			mixins     = undefined :: set()}).
-type(occi_resource() :: #occi_resource{}).

%%% OCCI Link
-record(occi_link, {id         = undefined :: uri(),
		    cid        = undefined :: occi_cid(),
		    title      = undefined :: binary(),
		    attributes = undefined :: dict(),   % orddict()
		    source                 :: uri(),
		    target                 :: uri(),
		    mixins     = undefined :: set()}).
-type(occi_link() :: #occi_link{}).

%%% OCCI Entity
-record(occi_entity, {id       = undefined :: uri(),
		      cid                  :: occi_cid()}).
-type(occi_entity() :: #occi_entity{}).

%%% OCCI Filter
-type(occi_filter() :: any()).

%%% OCCI hooks
-type(hook_type() :: {pid, pid()}).
-record(hook_handler, {id      = undefined    :: term(),
		       handler = undefined    :: hook_type()}).
-type(hook_handler() :: #hook_handler{}).

-record(occi_request, {collection             :: occi_collection(),
		       kinds      = []        :: [occi_kind()],
		       mixins     = []        :: [occi_mixin()],
		       action     = undefined :: occi_action(),
		       entities   = []        :: [occi_resource() | occi_link()]
		      }).
-type(occi_request() :: #occi_request{}).

-type(occi_node_id() :: uri()).
-type(occi_node_type() :: dir | 
			  undefined |
			  occi_query |
			  occi_resource | 
			  occi_link |
			  occi_user_mixin |
			  occi_collection).
-record(occi_node, {id                     :: occi_node_id(),
		    objid     = undefined  :: term(),
		    type      = undefined  :: occi_node_type(),
		    parent    = undefined  :: occi_node_id(),
		    data      = undefined  :: term(),
		    etag      = undefined  :: term(),
		    acl       = []         :: occi_acl()}).
-type(occi_node() :: #occi_node{}).

-record(occi_backend, {ref            :: atom(),
		       mod            :: atom(),
		       opts           :: term()}).
-type(occi_backend() :: #occi_backend{}).

-type(occi_acl() :: [term()]).

-type(occi_object() :: occi_node() | occi_entity() | occi_category() | occi_collection()).
