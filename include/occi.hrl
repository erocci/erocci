%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-type(occi_class() :: kind | mixin | action).

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
			 mixins         :: [occi_cid()]
			}).
-type(occi_extension() :: #occi_extension{}).

-record(occi_collection, {cid                    :: occi_cid(),
			  entities  = undefined  :: term()}). % ordset()
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
		    attributes          :: term(), % orddict()
		    actions             :: term()  % orddict()
		   }).
-type(occi_kind() :: #occi_kind{}).

-record(occi_mixin, {id                      :: #occi_cid{},
		     title       = undefined :: binary(),
		     location    = undefined :: uri(),
		     depends     = []        :: [#occi_cid{}],
		     applies     = []        :: [#occi_cid{}],
		     attributes              :: term(), % orddict()
		     actions                 :: term(), % orddict()
		     user        = false     :: boolean()
		    }).
-type(occi_mixin() :: #occi_mixin{}).

-type(occi_category() :: occi_kind() | occi_mixin() | occi_action()).

-record(occi_action, {id                        :: #occi_cid{},
		      title                     :: binary(),
		      location    = undefined   :: uri(),
		      attributes                :: term() % orddict()
		     }).
-type(occi_action() :: #occi_action{}).

%%% OCCI Attribute description
-type(occi_attr_key() :: atom()).
-record(occi_attr, {id                           :: occi_attr_key(),
		    type_id                      :: atom(),
		    f               = undefined  :: fun(),
		    title           = undefined  :: binary(),
		    properties                   :: [term()],
		    value           = undefined  :: any()}).
-type(occi_attr() :: #occi_attr{}).

%%% OCCI Resource
-record(occi_resource, {id         = undefined :: uri(),
			cid        = undefined :: occi_cid(),
		        summary    = undefined :: binary(),
			attributes = undefined :: term(),       % orddict()
			links                  :: term(),       % set()
			mixins     = undefined :: term()}).     % set()
-type(occi_resource() :: #occi_resource{}).

%%% OCCI Link
-record(occi_link, {id         = undefined :: uri(),
		    cid        = undefined :: occi_cid(),
		    attributes = undefined :: term(),           % orddict()
		    source                 :: uri(),
		    target                 :: uri(),
		    target_cid = undefined :: occi_cid(),
		    mixins     = undefined :: term()}).         % set()
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
-type(occi_node_objid() :: atom() | uri() | occi_cid()).
-type(occi_node_type() :: dir | 
			  mountpoint |
			  occi_query |
			  occi_resource | 
			  occi_link |
			  occi_user_mixin |
			  occi_collection).
-record(occi_node, {id                     :: occi_node_id(),
		    objid     = undefined  :: occi_node_objid(),
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
