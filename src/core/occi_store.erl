%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% 
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%% 
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%% 
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_store).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, register/1]).
-export([save/1,
	 update/1,
	 delete/1,
	 find/1,
	 load/1]).

%% supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).
-define(TABLE, ?MODULE).

-type(backend_ref() :: atom()).
-type(backend_mod() :: atom()).
-type(backend_opts() :: term()).
-type(backend_desc() :: {backend_ref(), backend_mod(), backend_opts()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

-spec register(backend_desc()) -> {ok, pid()} | ignore | {error, term()}.
register({Ref, Mod, Opts, Path}) when Path == "/" ->
    lager:info("Registering backend: ~p~n", [Ref]),
    Def = {Ref, {occi_backend, start_link, [Ref, Mod, [{ref, Ref} | Opts]]}, 
	   permanent, 5000, worker, [occi_backend, Mod]},
    case supervisor:start_child(occi_store, Def) of
	{ok, Pid} ->
	    Backend = #occi_backend{ref=Ref, mod=Mod, opts=Opts},
	    ets:insert(?TABLE, occi_node:new(occi_uri:parse(Path), Backend)),
	    {ok, Pid};
	{error, Err} ->
	    {error, Err}
    end;

register({_, _, _, _}) ->
    lager:error("Multiple backends not supported at this time.~n"),
    throw({error, not_supported}).

-spec save(occi_node()) -> ok | {error, term()}.
save(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:save(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	undefined ->
	    {error, undefined_backend};
	Backend ->
	    occi_backend:save(Backend, Node)
    end.

-spec update(occi_node()) -> ok | {error, term()}.
update(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:update(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	undefined ->
	    {error, undefined_backend};
	Backend ->
	    occi_backend:update(Backend, Node)
    end.

-spec delete(occi_node()) -> ok | {error, term()}.
delete(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:delete(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	undefined ->
	    {error, undefined_backend};
	Backend ->
	    occi_backend:delete(Backend, Node)
    end.

-spec find(occi_node()) -> {ok, occi_node()} | {error, term()}.
find(#occi_node{type=occi_query}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    {K, M, A} = occi_category_mgr:find_all(),
    UserMixins = get_user_mixins(),
    {ok, Req#occi_node{data={K, M++UserMixins, A}}};

find(#occi_node{type=occi_user_mixin}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    occi_backend:find(get_dft_backend(), Req);

find(#occi_node{id=#uri{path=Path}=Id}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    case occi_category_mgr:find(Id) of
	[] ->
	    occi_backend:find(get_backend(Path), Req);
	[#occi_kind{id=Cid}] ->
	    {ok, [occi_node:new(#uri{path=Path}, Cid)]};
	[#occi_mixin{id=Cid}] ->
	    {ok, [occi_node:new(#uri{path=Path}, Cid)]}
    end.

-spec load(occi_node()) -> occi_node().
load(#occi_node{id=#uri{path=Path}, data=undefined}=Node) ->
    lager:debug("occi_store:load(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	undefined ->
	    {error, undefined_backend};
	Backend ->
	    occi_backend:load(Backend, Node)
    end;
load(#occi_node{data=_}=Node) ->
    lager:debug("occi_store:load(~p)~n", [lager:pr(Node, ?MODULE)]),
    Node.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init([]) ->
    lager:info("Starting OCCI storage manager"),
    ?TABLE = ets:new(?TABLE, [set, public, {keypos, 2}, named_table]),
    % start no child, will be added with backends
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% internals
%%%===================================================================
-spec get_backend(uri()) -> atom().
get_backend(_) ->
    % TODO: handle multiple backends
    case ets:match_object(?TABLE, #occi_node{id=#uri{path="/", _='_'}, _='_'}) of
	[] ->
	    undefined;
	[#occi_node{data=#occi_backend{ref=Ref}}] ->
	    Ref
    end.

get_dft_backend() ->
    case ets:match_object(?TABLE, #occi_node{id=#uri{path="/", _='_'}, _='_'}) of
	[] ->
	    throw(no_default_backend);
	[#occi_node{data=#occi_backend{ref=Ref}}] ->
	    Ref
    end.

get_user_mixins() ->
    {ok, Mixins} = occi_backend:find(get_dft_backend(), #occi_node{type=occi_query, _='_'}),
    Mixins.
