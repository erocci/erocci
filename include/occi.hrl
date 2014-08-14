
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(occi_hrl).
-define(occi_hrl, true).

-define(xmlschema_ns, 'http://www.w3.org/2001/XMLSchema').

-define(scheme_core, 'http://schemas.ogf.org/occi/core#').
-define(cid_resource, #occi_cid{scheme=?scheme_core, term=resource}).
-define(cid_link, #occi_cid{scheme=?scheme_core, term=link}).

-type occi_marker() :: binary().
-type load_opt() :: {marker, occi_marker()}
		  | {limit, integer()}
		  | deep.
-type occi_schema() :: binary()
		     | occi_mixin()
		     | [{path, string()}].

-type occi_backend_capability() :: {schemas, [occi_schema()]}
				 | pagination
				 | deep.

-record(occi_env, {host, req}).
-type occi_env() :: #occi_env{}.

-type(occi_class() :: kind | mixin | action).
-type(occi_scheme() :: atom() | binary()).
-type(occi_term() :: atom() | binary()).

-define(scheme_to_atom(X), occi_known:scheme_to_atom(X)).
-define(term_to_atom(X), occi_known:term_to_atom(X)).
-define(class_to_atom(X), occi_known:class_to_atom(X)).
-define(attr_to_atom(X), occi_known:attr_to_atom(X)).

%%%% URI and al.
-record(uri, {scheme   = undefined    :: atom(),
	      userinfo = ""           :: string(),
	      host     = ""           :: string(),
	      port                    :: integer(),
	      path     = ""           :: string(),
	      query    = ""           :: string()}).
-type(uri() :: #uri{}).

%%% OCCI Extension
-record(occi_extension, {name           :: binary(),
			 scheme         :: occi_scheme(),
			 version        :: term(),
			 kinds          :: [occi_cid()],
			 mixins         :: [occi_cid()]
			}).
-type(occi_extension() :: #occi_extension{}).

-record(occi_collection, {id                     :: uri() | occi_cid(),
			  entities  = undefined  :: term()}). % ordset()
-type(occi_collection() :: #occi_collection{}).

%%% OCCI Category ID
-record(occi_cid, {scheme    = undefined :: atom() | binary(),
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
-type(occi_attr_key() :: atom() | binary()).
-type(occi_attr_type() :: {atom(), atom()}).
-record(occi_attr, {id                           :: occi_attr_key(),
		    type                         :: occi_attr_type(),
		    title           = undefined  :: binary(),
		    properties                   :: [term()],
		    value           = undefined  :: any()}).
-type(occi_attr() :: #occi_attr{}).

-type occi_objid() :: term().

%%% OCCI Resource
-record(occi_resource, {id         = undefined :: occi_objid(),       % internal id for backend
			cid        = undefined :: occi_cid(),
		        summary    = undefined :: binary(),
			attributes = undefined :: term(),       % orddict()
			links                  :: term(),       % set()
			mixins     = undefined :: term()}).     % set()
-type(occi_resource() :: #occi_resource{}).

%%% OCCI Link
-record(occi_link, {id         = undefined :: occi_objid(),           % internal id for backend
		    cid        = undefined :: occi_cid(),
		    attributes = undefined :: term(),           % orddict()
		    source                 :: uri(),
		    target                 :: uri(),
		    target_cid = undefined :: occi_cid(),
		    mixins     = undefined :: term()}).         % set()
-type(occi_link() :: #occi_link{}).

%%% OCCI Entity
-record(occi_entity, {id       = undefined :: occi_objid(),
		      cid                  :: occi_cid()}).
-type(occi_entity() :: #occi_entity{}).

%%% OCCI Filter
-type(occi_filter() :: occi_cid() | iolist() | {iolist(), iolist()}).
-type(occi_filters() :: occi_filter()).

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

-record(occi_user, {id             :: uid(),
		    groups         :: [gid()]}).
-type(occi_user() :: #occi_user{}).

-type(uid() :: integer()).
-type(gid() :: integer()).

-type(occi_node_id() :: uri()).
-type(occi_node_type() :: mountpoint |
			  capabilities |
			  occi_resource | 
			  occi_link |
			  occi_collection).
-record(occi_node, {id                     :: occi_node_id(),
		    objid     = undefined  :: occi_objid(),
		    type      = undefined  :: occi_node_type(),
		    data      = undefined  :: term(),
		    etag      = undefined  :: term(),
		    owner     = anonymous  :: term()}).
-type(occi_node() :: #occi_node{}).

-record(occi_backend, {ref            :: atom(),
		       mod            :: atom(),
		       mountpoint     :: uri(),
		       opts           :: term()}).
-type(occi_backend() :: #occi_backend{}).

-type(occi_object() :: occi_node() | occi_entity() | occi_category() | occi_collection()).

-type(acl() :: {acl_policy(), acl_op(), acl_node(), acl_user()}).

-type(acl_policy() :: allow | deny).
-type(acl_op() :: create | read | update | {action, binary() } | delete | '_').
-type(acl_node() :: capabilities | acl_url()).
-type(acl_url() :: binary()).
-type(acl_user() :: anonymous | authenticated | admin | owner | group | {group, binary() } | '_').

-endif.
