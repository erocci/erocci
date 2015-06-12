%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(occi_model_store_hrl).
-define(occi_model_store_hrl, true).

-type(occi_class() :: kind | mixin | action).
-type(occi_scheme() :: atom() | binary()).
-type(occi_term() :: atom() | binary()).

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
                          range                  :: {integer() | undefined, 
                                                     integer() | undefined, 
                                                     integer() | undefined},
                          marker    = undefined  :: binary(),
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

%%% OCCI store
-type occi_marker() :: binary() | integer().
-record(occi_store_opts, {node_f          = []        :: [occi_filter()],
                          entity_f        = []        :: [occi_filter()], 
                          deep            = false     :: boolean(), 
                          limit           = -1        :: integer(),
                          marker          = 0         :: occi_marker()}).
-type occi_store_opts() :: #occi_store_opts{}.

-record(occi_store_ctx, {user     = anonymous    :: occi_user(),
                         auth_ref = undefined    :: term()}).
-type occi_store_ctx() :: #occi_store_ctx{}.

-record(occi_user, {id             :: uid(),
                    groups         :: [gid()]}).
-type(occi_user() :: #occi_user{}).

-type(uid() :: integer()).
-type(gid() :: integer()).

%%% OCCI Filter
-type(occi_filter() :: occi_cid() | iolist() | {iolist(), iolist()} | fun()).
-type(occi_filters() :: occi_filter()).

-endif.