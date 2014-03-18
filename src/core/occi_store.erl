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
    Mp = occi_node:new(occi_uri:parse([$/ | Path]), 
		       #occi_backend{ref=Ref, mod=Mod, opts=Opts}),
    Def = {Ref, {occi_backend, start_link, [Mp, Opts]}, 
	   permanent, 5000, worker, [occi_backend, Mod]},
    case supervisor:start_child(occi_store, Def) of
	{ok, Pid} ->
	    add_backend(Mp),
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
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    occi_backend:save(Ref, occi_mixin:rm_prefix(Mixin, Prefix));
	{error, Err} ->
	    {error, Err}
    end;

save(#occi_node{type=occi_collection}=Node) ->
    lager:debug("occi_store:save(~p)~n", [lager:pr(Node, ?MODULE)]),
    Merge = fun (_B, ok, ok) -> ok;
		(_B, _, {error, Err}) -> {error, Err};
		(_B, {error, Err}, _) -> {error, Err}
	    end,
    Reqs = filter_collection(Node),
    fold(Merge, ok, save, Reqs);    

save(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:save(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    occi_backend:save(Ref, occi_node:rm_prefix(Node, Prefix));
	{error, Err} ->
	    {error, Err}
    end.

-spec update(occi_node()) -> ok | {error, term()}.
update(#occi_node{type=occi_collection}=Node) ->
    lager:debug("occi_store:update(~p)~n", [lager:pr(Node, ?MODULE)]),
    Tags = cast(update, filter_collection(Node)),
    wait_response(Tags);

update(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:update(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    occi_backend:update(Ref, occi_node:rm_prefix(Node, Prefix));
	{error, Err} ->
	    {error, Err}
    end.

-spec delete(occi_node() | occi_mixin()) -> ok | {error, term()}.
delete(#occi_mixin{location=#uri{path=Path}}=Mixin) ->
    lager:debug("occi_store:delete(~p)~n", [lager:pr(Mixin, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    occi_backend:delete(Ref, occi_mixin:rm_prefix(Mixin, Prefix));
	{error, Err} ->
	    {error, Err}
    end;

delete(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:delete(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    occi_backend:delete(Ref, occi_node:rm_prefix(Node, Prefix));
	{error, Err} ->
	    {error, Err}
    end.

-spec find(occi_node() | occi_cid() | occi_category()) -> {ok, [occi_node() | occi_category()]} | {error, term()}.
find(#occi_cid{class=Cls}=Cid) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Cid, ?MODULE)]),
    if
	Cls == kind orelse Cls == action ->
	    {ok, occi_category_mgr:find(Cid)};
	Cls == mixin orelse Cls == '_' ->
	    Merge = fun (#occi_node{id=#uri{path=Prefix}}, Mixins, Acc) ->
			    lists:map(fun (Mixin) ->
					      occi_mixin:add_prefix(Mixin, Prefix)
				      end, Mixins) ++ Acc
		    end,
	    case fold(Merge, [], find, #occi_mixin{id=Cid, _='_'}) of
		{ok, UMixins} ->
		    {ok, occi_category_mgr:find(Cid) ++ UMixins};
		{error, Err} ->
		    {error, Err}
	    end;
	true ->
	    {error, invalid_class}
    end;

find(#occi_kind{}=Kind) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Kind, ?MODULE)]),
    occi_category_mgr:find(Kind);

find(#occi_action{}=Action) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Action, ?MODULE)]),
    occi_category_mgr:find(Action);

find(#occi_mixin{location='_'}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    Merge = fun (#occi_node{id=#uri{path=Prefix}}, Mixins, Acc) ->
		    lists:map(fun (Mixin) ->
				      occi_mixin:add_prefix(Mixin, Prefix)
			      end, Mixins) ++ Acc
	    end,
    fold(Merge, [], find, Req);

find(#occi_mixin{location=#uri{path=Path}}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    case occi_backend:find(Ref, occi_mixin:rm_prefix(Req, Prefix)) of
		{ok, Res} ->
		    {ok, lists:map(fun (E) -> occi_mixin:add_prefix(E, Prefix) end, Res)};
		{error, Err} ->
		    {error, Err}
	    end;
	{error, Err} ->
	    {error, Err}
    end;

find(#occi_node{type=occi_query}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    {K, M, A} = occi_category_mgr:find_all(),
    Merge = fun (#occi_node{id=#uri{path=Prefix}}, Mixins, Acc) ->
		    Acc ++ lists:map(fun (M2) ->
					     occi_mixin:add_prefix(M2, Prefix)
				     end, Mixins)
	    end,
    case fold(Merge, M, find, #occi_mixin{_='_'}) of
	{ok, M3} ->
	    {ok, [Req#occi_node{data={K, M3, A}}]};
	{error, Err} ->
	    {error, Err}
    end;

find(#occi_node{id=#uri{path=Path}=Id}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    case occi_category_mgr:find(Id) of
	[] ->
	    case get_backend(Path) of
		{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
		    case occi_backend:find(Ref, occi_node:rm_prefix(Req, Prefix)) of
			{ok, Res} ->
			    {ok, lists:map(fun(E) -> occi_node:add_prefix(E, Prefix) end, Res)};
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
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    case occi_backend:load(Ref, occi_node:rm_prefix(Node, Prefix)) of
		{ok, Node2} ->
		    {ok, occi_node:add_prefix(Node2, Prefix)};
		{error, Err} ->
		    {error, Err}
	    end;
	{error, Err} ->
	    {error, Err}
    end;

load(#occi_node{objid=#occi_cid{}=Cid, type=occi_collection, data=undefined}=Node) ->
    lager:debug("occi_store:load(~p)~n", [lager:pr(Node, ?MODULE)]),
    Merge = fun (_B, #occi_node{data=undefined}, Acc) ->
		    Acc;
		(#occi_node{id=#uri{path=Prefix}}, #occi_node{data=C}, Acc) ->
		    occi_collection:merge(Acc, occi_collection:add_prefix(C, Prefix))
	    end,
    case fold(Merge, occi_collection:new(Cid), load, Node) of
	{ok, Coll} ->
	    {ok, Node#occi_node{data=Coll}};
	{error, Err} ->
	    {error, Err}
    end;

load(#occi_node{id=#uri{path=Path}, data=undefined}=Node) ->
    lager:debug("occi_store:load(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    case occi_backend:load(Ref, occi_node:rm_prefix(Node, Prefix)) of
		{ok, Node2} ->
		    {ok, occi_node:add_prefix(Node2, Prefix)};
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
		{ok, #occi_node{}=B} ->
		    {ok, B};
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

fold(Merge, Res, Op, Req) ->
    Reqs = gb_sets:fold(fun (Backend, Acc) ->
				[{Backend, Req}|Acc]
			end, [], get_mounts()),
    Tags = cast(Op, Reqs),
    wait_response(Merge, Res, Tags).

cast(Op, Reqs) ->
    lists:foldl(fun ({#occi_node{objid=Ref}=Backend, Req}, Acc) ->
			gb_trees:insert(occi_backend:cast(Ref, Op, Req), Backend, Acc)
		end, gb_trees:empty(), Reqs).

wait_response(Tags) ->
    receive 
	{Tag, ok} ->
	    Tags2 = gb_trees:delete(Tag, Tags),
	    case gb_trees:is_empty(Tags2) of
		true -> 
		    ok;
		false ->
		    wait_response(Tags2)
	    end;
	{_Tag, {error, Err}} ->
	    cancel_response(gb_trees:next(gb_trees:iterator(Tags))),
	    {error, Err}
    after 
	5000 ->
	    cancel_response(gb_trees:next(gb_trees:iterator(Tags))),
	    {error, timeout}
    end.

wait_response(Merge, Acc, Tags) ->
    receive
	{Tag, {ok, Res}} ->
	    Acc2 = Merge(gb_trees:get(Tag, Tags), Res, Acc),
	    Tags2 = gb_trees:delete(Tag, Tags),
	    case gb_trees:is_empty(Tags2) of
		true -> 
		    {ok, Acc2};
		false ->
		    wait_response(Merge, Acc2, Tags2)
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

%%%
%%% return list of {#occi_node{type=occi_collection}, mountpoint}
%%% each collection only contains entities related to the mountpoint
%%%
filter_collection(#occi_node{type=occi_collection, data=#occi_collection{cid=Cid, entities=E}}=Node) ->
    F = fun (#uri{path=Path}=Uri, Acc) ->
		case get_backend(Path) of
		    {error, Err} -> throw({error, Err});
		    {ok, #occi_node{id=#uri{path=Prefix}}=Backend} ->
			case gb_trees:lookup(Backend, Acc) of
			    none ->
				C = occi_collection:new(Cid, [occi_uri:rm_prefix(Uri, Prefix)]),
				gb_trees:insert(Backend, Node#occi_node{data=C}, Acc);
			    {value, #occi_collection{}=C} ->
				C2 = occi_collection:add_entity(C, occi_uri:rm_prefix(Uri, Prefix)),
				gb_trees:insert(Backend, Node#occi_node{data=C2}, Acc)
			end
		end
	end,
    gb_trees:to_list(ordsets:fold(F, gb_trees:empty(), E)).
