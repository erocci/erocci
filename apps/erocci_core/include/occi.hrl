%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013-2015, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(occi_hrl).
-define(occi_hrl, true).

-include("occi_log.hrl").
-include("occi_model_store.hrl"). 
-include("occi_acl.hrl").

-define(xmlschema_ns, 'http://www.w3.org/2001/XMLSchema').

-define(scheme_core, 'http://schemas.ogf.org/occi/core#').
-define(cid_resource, #occi_cid{scheme=?scheme_core, term=resource}).
-define(cid_link, #occi_cid{scheme=?scheme_core, term=link}).

-type occi_schema() :: XmlExt :: binary()
		     | occi_mixin()
		     | [{path, Path :: string()}].

-type occi_backend_capability() :: {schemas, [occi_schema()]}
          | pagination
          | deep.

-record(occi_env, {req_uri, req}).
-type occi_env() :: #occi_env{}.

-define(scheme_to_atom(X), occi_known:scheme_to_atom(X)).
-define(term_to_atom(X), occi_known:term_to_atom(X)).
-define(class_to_atom(X), occi_known:class_to_atom(X)).
-define(attr_to_atom(X), occi_known:attr_to_atom(X)).

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

-endif.
