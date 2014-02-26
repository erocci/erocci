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
%%% @doc occi_store uses 2 structures to store mountpoints and associated
%%%      backends. These structures are stored in ets table:
%%%
%%%      {set, gb_set()} : a set of occi_node{type=mountpoint}. Used to quickly
%%%                        iterate over mountpoints
%%%      {tree, gb_tree()} : a tree of {Length, [occi_node()]} where Length is 
%%%                          the length of '/' splitted path array.
%%%                          The structure is used to get a backend given a path.
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
register({Ref, Mod, Opts, [ $/ | Path ]}) ->
    lager:info("Registering backend: ~p~n", [Ref]),
    Def = {Ref, {occi_backend, start_link, [Ref, Mod, [{ref, Ref} | Opts]]}, 
	   permanent, 5000, worker, [occi_backend, Mod]},
    case supervisor:start_child(occi_store, Def) of
	{ok, Pid} ->
	    Backend = #occi_backend{ref=Ref, mod=Mod, opts=Opts},
	    add_backend(occi_node:new(occi_uri:parse([$/ | Path]), Backend)),
	    {ok, Pid};
	{error, Err} ->
	    {error, Err}
    end;

register({_, _, _, Path}) ->
    lager:error("Invalid mountpoint: ~p~n", [Path]),
    throw({invalid_mountpoint, Path}).

-spec save(occi_node() | occi_mixin()) -> ok | {error, term()}.
save(#occi_mixin{location=#uri{path=Path}}=Mixin) ->
    lager:debug("occi_store:save(~p)~n", [lager:pr(Mixin, ?MODULE)]),
    case get_backend(Path) of
	{ok, Ref, Prefix} ->
	    occi_backend:save(Ref, rm_prefix(Prefix, Mixin));
	{error, Err} ->
	    {error, Err}
    end;

save(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:save(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, Ref, Prefix} ->
	    occi_backend:save(Ref, rm_prefix(Prefix, Node));
	{error, Err} ->
	    {error, Err}
    end.

-spec update(occi_node()) -> ok | {error, term()}.
update(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:update(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, Ref, Prefix} ->
	    occi_backend:update(Ref, rm_prefix(Prefix, Node));
	{error, Err} ->
	    {error, Err}
    end.

-spec delete(occi_node() | occi_mixin()) -> ok | {error, term()}.
delete(#occi_mixin{location=#uri{path=Path}}=Mixin) ->
    lager:debug("occi_store:delete(~p)~n", [lager:pr(Mixin, ?MODULE)]),
    case get_backend(Path) of
	{ok, Ref, Prefix} ->
	    occi_backend:delete(Ref, rm_prefix(Prefix, Mixin));
	{error, Err} ->
	    {error, Err}
    end;

delete(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:delete(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, Ref, Prefix} ->
	    occi_backend:delete(Ref, rm_prefix(Prefix, Node));
	{error, Err} ->
	    {error, Err}
    end.

-spec find(occi_node() | occi_mixin()) -> {ok, [occi_node() | occi_mixin()]} | {error, term()}.
find(#occi_mixin{location='_'}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    Merge = fun (#occi_node{id=#uri{path=Prefix}}, Mixins, Acc) ->
		    lists:map(fun (Mixin) ->
				      add_prefix(Prefix, Mixin)
			      end, Mixins) ++ Acc
	    end,
    store_fold(Merge, [], find, Req);

find(#occi_mixin{location=#uri{path=Path}}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    case get_backend(Path) of
	{ok, Ref, Prefix} ->
	    case occi_backend:find(Ref, rm_prefix(Prefix, Req)) of
		{ok, Res} ->
		    {ok, lists:map(fun (E) -> add_prefix(Prefix, E) end, Res)};
		{error, Err} ->
		    {error, Err}
	    end;
	{error, Err} ->
	    {error, Err}
    end;

find(#occi_node{type=occi_query}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    {K, M, A} = occi_category_mgr:find_all(),
    Merge = fun (_B, Mixins, Acc) ->
		    Mixins ++ Acc
	    end,
    case store_fold(Merge, M, find, #occi_mixin{_='_'}) of
	{ok, M2} ->
	    {ok, [Req#occi_node{data={K, M2, A}}]};
	{error, Err} ->
	    {error, Err}
    end;

find(#occi_node{id=#uri{path=Path}=Id}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    case occi_category_mgr:find(Id) of
	[] ->
	    case get_backend(Path) of
		{ok, Ref, Prefix} ->
		    case occi_backend:find(Ref, rm_prefix(Prefix, Req)) of
			{ok, Res} ->
			    {ok, lists:map(fun(E) -> add_prefix(Prefix, E) end, Res)};
			{error, Err} ->
			    {error, Err}
		    end;
		{error, Err} ->
		    {error, Err}
	    end;
	[#occi_kind{id=Cid}] ->
	    {ok, [occi_node:new(#uri{path=Path}, Cid)]};
	[#occi_mixin{id=Cid}] ->
	    {ok, [occi_node:new(#uri{path=Path}, Cid)]}
    end.

-spec load(occi_node()) -> {ok, occi_node()} | {error, term()}.
load(#occi_node{id=#uri{path=Path}, type=dir}=Node) ->
    lager:debug("occi_store:load(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, Ref, Prefix} ->
	    case occi_backend:load(Ref, rm_prefix(Prefix, Node)) of
		{ok, Node2} ->
		    {ok, add_prefix(Prefix, Node2)};
		{error, Err} ->
		    {error, Err}
	    end;
	{error, Err} ->
	    {error, Err}
    end;
load(#occi_node{objid=#occi_cid{}=Cid, type=occi_collection}=Node) ->
    lager:debug("occi_store:load(~p)~n", [lager:pr(Node, ?MODULE)]),
    Merge = fun (#occi_node{id=#uri{path=Prefix}}, #occi_node{data=C}, Acc) ->
		    occi_collection:merge(Acc, C, Prefix)
	    end,
    case store_fold(Merge, occi_collection:new(Cid), load, Node) of
	{ok, Coll} ->
	    {ok, Node#occi_node{data=Coll}};
	{error, Err} ->
	    {error, Err}
    end;
load(#occi_node{id=#uri{path=Path}, data=undefined}=Node) ->
    lager:debug("occi_store:load(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, Ref, Prefix} ->
	    case occi_backend:load(Ref, rm_prefix(Prefix, Node)) of
		{ok, Node2} ->
		    {ok, add_prefix(Prefix, Node2)};
		{error, Err} ->
		    {error, Err}
	    end;
	{error, Err} ->
	    {error, Err}
    end;
load(#occi_node{data=_}=Node) ->
    lager:debug("occi_store:load(~p)~n", [lager:pr(Node, ?MODULE)]),
    {ok, Node}.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init([]) ->
    lager:info("Starting OCCI storage manager"),
    ?TABLE = ets:new(?TABLE, [set, public, {keypos, 1}, named_table]),
    ets:insert(?TABLE, {tree, gb_trees:empty()}),
    ets:insert(?TABLE, {set, gb_sets:new()}),
    % start no child, will be added with backends
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% internals
%%%===================================================================
-spec get_mounts() -> set().
get_mounts() ->
    ets:lookup_element(?TABLE, set, 2).

-spec get_backend(uri()) -> atom().
get_backend(Path) when is_list(Path) ->
    T = ets:lookup_element(?TABLE, tree, 2),
    get_backend2(Path, T).

get_backend2(Path, Tree) when is_list(Path) ->
    case gb_trees:is_empty(Tree) of
	true ->
	    {error, no_default_backend};
	false->
	    {_L, Mounts, Tree2} = gb_trees:take_largest(Tree),
	    case lookup_mountpoint(Path, Mounts) of
		{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
		    {ok, Ref, Prefix};
		none ->
		    get_backend2(Path, Tree2)
	    end
    end.

-spec lookup_mountpoint(string(), [occi_node()]) -> {ok, occi_node()} | none.
lookup_mountpoint(_Path, []) ->
    none;
lookup_mountpoint(Path, [#occi_node{id=#uri{path=Mountpoint}}=Node|Tail]) ->
    case lists:prefix(Mountpoint, Path) of
	true -> {ok, Node};
	false -> lookup_mountpoint(Path, Tail)
    end.

add_backend(Node) ->
    insert_tree(Node),
    insert_set(Node).

insert_tree(#occi_node{id=#uri{path=Path}, type=mountpoint}=Mp) ->
    L = length(string:tokens(Path, "/")),
    T = ets:lookup_element(?TABLE, tree, 2),
    Mps = case gb_trees:lookup(L, T) of
	      {value, Val} -> Val;
	      none ->
		  []
	  end,
    T1 = gb_trees:enter(L, [Mp | Mps], T),
    ets:insert(?TABLE, {tree, T1}).

insert_set(#occi_node{type=mountpoint}=Mp) ->
    S = ets:lookup_element(?TABLE, set, 2),
    ets:insert(?TABLE, {set, gb_sets:add(Mp, S)}).

store_fold(Merge, Res, Op, Req) ->
    store_fold(Merge, Res, Op, Req, get_mounts()).

store_fold(Merge, Res, Op, Req, Mounts) ->
    Tags = gb_sets:fold(fun (#occi_node{objid=Ref}=Mp, Acc) ->
				gb_trees:insert(occi_backend:cast(Ref, Op, Req), Mp, Acc)
			end, gb_trees:empty(), Mounts),
    wait_response(Merge, Res, Tags, Mounts).

wait_response(Merge, Acc, Tags, Mounts) ->
    receive 
	{Tag, {ok, Res}} ->
	    Acc2 = Merge(gb_trees:get(Tag, Tags), Res, Acc),
	    Tags2 = gb_trees:delete(Tag, Tags),
	    case gb_trees:is_empty(Tags2) of
		true -> 
		    {ok, Acc2};
		false ->
		    wait_response(Merge, Acc2, Tags2, Mounts)
	    end;
	{_Tag, {error, Err}} ->
	    cancel_response(gb_trees:next(gb_trees:iterator(Tags))),
	    {error, Err}
    after 
	5000 ->
	    cancel_response(gb_trees:next(gb_trees:iterator(Tags))),
	    {error, timeout}
    end.

cancel_response(none) ->
    ok;
cancel_response({Tag, #occi_node{objid=Ref}, It}) ->
    occi_backend:cancel(Ref, Tag),
    cancel_response(gb_trees:next(It)).

rm_prefix("/", E) ->
    E;
rm_prefix(Prefix, #occi_mixin{location=#uri{path=Path}=Uri}=Mixin) when is_list(Prefix) ->
    Mixin#occi_mixin{location=Uri#uri{path=rm_substr(Prefix, Path)}};
rm_prefix(Prefix, #occi_node{id=#uri{path=Path}=Uri}=Node) when is_list(Prefix) ->
    Node#occi_node{id=Uri#uri{path=rm_substr(Prefix, Path)}}.

rm_substr([C|S1], [C|S2]) ->
    rm_substr(S1, S2);
rm_substr(_S1, S2) ->
    S2.

add_prefix("/", E) ->
    E;

add_prefix(Prefix, #occi_mixin{location=#uri{path=Path}=Uri}=Mixin) ->
    Mixin#occi_mixin{location=Uri#uri{path=Prefix=Path}};

add_prefix(Prefix, #occi_node{id=#uri{path=Path}=Uri, type=occi_resource, 
			      data=#occi_resource{id=Id}=Res}=Node) ->
    Res2 = Res#occi_resource{id=Id#uri{path=Prefix++Id#uri.path}},
    Node#occi_node{id=Uri#uri{path=Prefix++Path}, data=Res2};

add_prefix(Prefix, #occi_node{id=#uri{path=Path}=Uri, type=occi_link, 
			      data=#occi_link{id=Id}=Link}=Node) ->
    Link2 = Link#occi_link{id=Id#uri{path=Prefix++Id#uri.path}},
    Node#occi_node{id=Uri#uri{path=Prefix++Path}, data=Link2};

add_prefix(Prefix, #occi_node{id=#uri{path=Path}=Uri}=Node) ->
    Node#occi_node{id=Uri#uri{path=Prefix++Path}}.
