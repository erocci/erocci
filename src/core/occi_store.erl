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
-export([get/1]).
-export([save/1,
	 update/1,
	 delete/1,
	 find/1,
	 find/2,
	 load/1,
	 action/2]).

%% supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).
-define(TABLE, ?MODULE).

-type(backend_ref() :: atom()).
-type(backend_mod() :: atom()).
-type(backend_opts() :: term()).
-type(backend_mountpoint() :: binary() | string()).
-type(backend_desc() :: {backend_ref(), 
			 backend_mod(), 
			 backend_opts(),
			 backend_mountpoint()}).

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
register({Ref, Mod, Opts, Path}) when is_list(Path) ->
    register({Ref, Mod, Opts, list_to_binary(Path)});
register({Ref, Mod, Opts, Path}) when is_atom(Ref), is_atom(Mod), is_binary(Path) ->
    lager:info("Registering backend: ~p~n", [Ref]),
    Id = occi_uri:parse(Path),
    Backend = #occi_backend{ref=Ref, mod=Mod, mountpoint=Id, opts=Opts},
    Mp = occi_node:new(Id, Backend),
    Def = {Ref, {occi_backend, start_link, [Backend]}, 
	   permanent, 5000, worker, [occi_backend, Mod]},
    case supervisor:start_child(occi_store, Def) of
	{ok, Pid} ->
	    add_backend(Mp),
	    load_user_mixins(Mp),
	    {ok, Pid};
	{error, {already_started, Pid}} ->
	    {ok, Pid};
	{error, Err} ->
	    {error, Err}
    end.

-spec save(occi_node()) -> ok | {error, term()}.
save(#occi_node{type=occi_collection, objid=#occi_cid{}}=Node) ->
    lager:debug("occi_store:save(~p)~n", [lager:pr(Node, ?MODULE)]),
    cast(save, filter_collection(Node));

save(#occi_node{id=#uri{path=Path}, type=occi_resource}=Node) ->
    lager:debug("occi_store:save(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    {ResNode, LinksNodes} = process_resource_links(Node),
	    occi_backend:save(Ref, occi_node:rm_prefix(ResNode, Prefix)),
	    save_resource_links(LinksNodes);
	{error, Err} ->
	    {error, Err}
    end;

save(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:save(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    occi_backend:save(Ref, occi_node:rm_prefix(Node, Prefix));
	{error, Err} ->
	    {error, Err}
    end.


-spec update(occi_node()) -> ok | {error, term()}.
update(#occi_node{id=#uri{path=Path}, type=capabilities, data=#occi_mixin{}=Mixin}=Node) ->
    lager:debug("occi_store:update(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    Res = occi_backend:update(Ref, occi_node:rm_prefix(Node, Prefix)),
	    occi_category_mgr:register_mixin(Mixin),
	    Res;
	{error, Err} ->
	    {error, Err}
    end;

update(#occi_node{type=occi_collection, objid=#occi_cid{}}=Node) ->
    lager:debug("occi_store:update(~p)~n", [lager:pr(Node, ?MODULE)]),
    cast(update, filter_collection(Node));

update(#occi_node{id=#uri{path=Path}}=Node) ->
    lager:debug("occi_store:update(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    occi_backend:update(Ref, occi_node:rm_prefix(Node, Prefix));
	{error, Err} ->
	    {error, Err}
    end.

-spec delete(occi_node()) -> ok | {error, term()}.
delete(#occi_node{type=occi_collection, objid=#occi_cid{}, data=undefined}=Node) ->
    case load(Node) of
	{ok, N2} -> delete(N2);
	{error, Err} -> {error, Err}
    end;

delete(#occi_node{type=occi_collection, objid=#occi_cid{}}=Node) ->
    lager:debug("occi_store:delete(~p)~n", [lager:pr(Node, ?MODULE)]),
    cast(delete, filter_collection(Node));

delete(#occi_node{id=#uri{path=Path}, type=capabilities, data=#occi_mixin{}=Mixin}=Node) ->
    lager:debug("occi_store:delete(~p)~n", [lager:pr(Node, ?MODULE)]),
    case get_backend(Path) of
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    Res = occi_backend:delete(Ref, occi_node:rm_prefix(Node, Prefix)),
	    occi_category_mgr:unregister_mixin(Mixin),
	    Res;
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
    end;

delete(_) ->
    {error, unsupported_node}.

-spec get(occi_cid()) -> {ok, occi_category()} | {error, term()}.
get(Cid) ->
    occi_category_mgr:get(Cid).

-spec find(occi_node() | occi_cid()) -> {ok, [occi_node()]} | {error, term()}.
find(#occi_node{type=capabilities, objid=#occi_cid{}=Cid}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    case occi_category_mgr:find(Cid) of
	[#occi_mixin{location=#uri{path=Path}}] ->
	    case get_backend(Path) of
		{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
		    case occi_backend:find(Ref, Req) of
			{ok, [#occi_node{}=Node]} ->
			    {ok, [occi_node:add_prefix(Node, Prefix)]};
			{ok, []} ->
			    {ok, [occi_capabilities:new()]};
			{error, Err} ->
			    {error, Err}
		    end;
		{error, Err} ->
		    {error, Err}
	    end;
	_ ->
	    {ok, []}
    end;

find(#occi_node{type=capabilities}=Req) ->
    lager:debug("occi_store:find(~p)~n", [lager:pr(Req, ?MODULE)]),
    {K, M, A} = occi_category_mgr:find_all(),
    {ok, [occi_capabilities:new(K, M, A)]};

find(Req) ->
    find(Req, []).


-spec find(occi_node(), occi_filters()) -> {ok, occi_node()} | none | {error, term()}.
find(#occi_node{type=occi_collection, objid=#occi_cid{}=Cid}=Req, _Filters) ->
    lager:debug("occi_store:find(~p, ~p)~n", [lager:pr(Req, ?MODULE), _Filters]),
    case find(Cid) of
	{ok, #occi_kind{id=Id}} -> 
	    {ok, [Req#occi_node{objid=Id}]};
	{ok, [#occi_mixin{id=Id}]} ->
	    {ok, [Req#occi_node{objid=Id}]};
	{ok, _} ->
	    none;
	{error, Err} ->
	    {error, Err}
    end;

find(#occi_node{id=undefined}, _Filters) ->
    {ok, []};

find(#occi_node{id=#uri{path=Path}=Id}=Req, _Filters) ->
    lager:debug("occi_store:find(~p, ~p)~n", [lager:pr(Req, ?MODULE), _Filters]),
    case occi_category_mgr:find(Id) of
	[] ->
	    case get_backend(Path) of
		{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
		    case occi_backend:find(Ref, occi_node:rm_prefix(Req, Prefix)) of
			{ok, Res} ->
			    {ok, lists:map(fun (E) -> occi_node:add_prefix(E, Prefix) end, Res)};
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
load(#occi_node{type=capabilities, data=undefined}=Req) ->
    {ok, [Res]} = find(Req),
    {ok, Res};

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

action(#occi_node{type=capabilities}=_N, _A) ->
    lager:debug("occi_store:action(~p, ~p)~n", [lager:pr(_N, ?MODULE), lager:pr(_A, ?MODULE)]),
    {error, unsupported_node};

action(#occi_node{type=occi_collection, data=undefined}=Node, A) ->
    lager:debug("occi_store:action(~p, ~p)~n", [lager:pr(Node, ?MODULE), lager:pr(A, ?MODULE)]),
    case load(Node) of
	{ok, N2} -> action(N2, A);
	{error, Err} -> {error, Err}
    end;

action(#occi_node{type=occi_collection, data=#occi_collection{entities=E}}=_N, #occi_action{}=A) ->
    lager:debug("occi_store:action(~p, ~p)~n", [lager:pr(_N, ?MODULE), lager:pr(A, ?MODULE)]),
    Reqs = ordsets:fold(fun (#uri{path=Path}=Uri, Acc) ->
				case get_backend(Path) of
				    {error, Err} -> throw({error, Err});
				    {ok, #occi_node{id=#uri{path=Prefix}}=Backend} ->
					Req = {occi_uri:rm_prefix(Uri, Prefix), A},
					[{Backend, Req} | Acc]
				end
			end, [], E),
    cast(action, Reqs);

action(#occi_node{type=dir}=Node, #occi_action{}=A) ->
    lager:debug("occi_store:action(~p, ~p)~n", [lager:pr(Node, ?MODULE), lager:pr(A, ?MODULE)]),
    Reqs = build_dir_reqs(Node, A),
    cast(action, Reqs);

action(#occi_node{objid=#uri{path=Path}=Id}=_N, #occi_action{}=A) ->
    lager:debug("occi_store:action(~p, ~p)~n", [lager:pr(_N, ?MODULE), lager:pr(A, ?MODULE)]),
    case get_backend(Path) of
	{error, Err} -> throw({error, Err});
	{ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
	    occi_backend:action(Ref, occi_uri:rm_prefix(Id, Prefix), A)
    end.

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
-spec get_mounts() -> term(). % set()
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

fold(Merge, Acc0, Op, Req) ->
    Reqs = gb_sets:fold(fun (Backend, Acc) ->
				[{Backend, Req}|Acc]
			end, [], get_mounts()),
    cast(Merge, Acc0, Op, Reqs).

cast(Op, Reqs) ->
    cast(noop, [], Op, Reqs).

cast(Merge, Acc0, Op, Reqs) ->
    Tags = lists:foldl(fun ({#occi_node{objid=Ref}=Backend, Req}, Acc) ->
			       gb_trees:insert(occi_backend:cast(Ref, Op, Req), Backend, Acc)
		       end, gb_trees:empty(), Reqs),
    wait_response(Merge, Acc0, Tags).

wait_response(Merge, Acc, Tags) ->
    receive
	{Tag, ok} ->
	    Tags2 = gb_trees:delete(Tag, Tags),
	    case gb_trees:is_empty(Tags2) of
		true -> ok;
		false -> wait_response(Merge, Acc, Tags2)
	    end;
	{Tag, {ok, Res}} ->
	    case Merge(gb_trees:get(Tag, Tags), Res, Acc) of
		{error, Err} ->
		    cancel_response(gb_trees:next(gb_trees:iterator(Tags))),
		    {error, Err};
		{halt, Acc2} ->
		    cancel_response(gb_trees:next(gb_trees:iterator(Tags))),
		    {ok, Acc2};
		Acc2 ->
		    Tags2 = gb_trees:delete(Tag, Tags),
		    case gb_trees:is_empty(Tags2) of
			true -> 
			    {ok, Acc2};
			false ->
			    wait_response(Merge, Acc2, Tags2)
		    end
	    end;
	{_Tag, {error, Err}} ->
	    cancel_response(gb_trees:next(gb_trees:iterator(Tags))),
	    {error, Err};
	_O ->
	    cancel_response(gb_trees:next(gb_trees:iterator(Tags))),
	    {error, {unexpected_message, _O}}
    after 
	occi_config:get(backend_timeout, 5000) ->
	    cancel_response(gb_trees:next(gb_trees:iterator(Tags))),
	    {error, timeout}
    end.

cancel_response(none) ->
    ok;
cancel_response({Tag, #occi_node{objid=Ref}, It}) ->
    occi_backend:cancel(Ref, Tag),
    cancel_response(gb_trees:next(It)).

%%%
%%% return list of {mountpoint, #occi_node{type=occi_collection}}
%%% each collection only contains entities related to the mountpoint
%%%
filter_collection(#occi_node{type=occi_collection, data=#occi_collection{id=Cid, entities=E}}=Node) ->
    F = fun (#uri{path=Path}=Uri, Acc) ->
		case get_backend(Path) of
		    {error, Err} -> throw({error, Err});
		    {ok, #occi_node{id=#uri{path=Prefix}}=Backend} ->
			case gb_trees:lookup(Backend, Acc) of
			    none ->
				C = occi_collection:new(Cid, [occi_uri:rm_prefix(Uri, Prefix)]),
				gb_trees:insert(Backend, Node#occi_node{data=C}, Acc);
			    {value, #occi_node{data=#occi_collection{}=C}} ->
				C2 = occi_collection:add_entity(C, occi_uri:rm_prefix(Uri, Prefix)),
				gb_trees:update(Backend, Node#occi_node{data=C2}, Acc)
			end
		end
	end,
    gb_trees:to_list(ordsets:fold(F, gb_trees:empty(), E)).

build_dir_reqs(#occi_node{type=dir, data=Children}, A) ->
    gb_sets:fold(fun (#occi_node{type=dir}=Child, Acc) ->
			 Acc ++ build_dir_reqs(Child, A);
		     (#uri{path=Path}=Uri, Acc) ->
			 case get_backend(Path) of
			     {error, Err} -> throw({error, Err});
			     {ok, #occi_node{id=#uri{path=Prefix}}=Backend} ->
				 [{Backend, {occi_uri:rm_prefix(Uri, Prefix), A}} | Acc]
			 end
		 end, [], Children).

load_user_mixins(#occi_node{objid=Ref}) ->
    case occi_backend:find(Ref, #occi_node{type=capabilities, _='_'}) of
	{ok, [#occi_node{data={_, Mixins, _}}]} ->
	    lists:foreach(fun (Mixin) -> 
				  occi_category_mgr:register_mixin(Mixin)
			  end, Mixins);
	{error, Err} ->
	    {error, Err}
    end.

process_resource_links(#occi_node{id=ResId, owner=Owner, data=Res}=Node) ->
    {Links, LinksNodes} = lists:foldl(fun (#uri{}=Link, {AccUris, AccNodes}) ->
					      { sets:add_element(Link, AccUris), AccNodes };
					  (#occi_link{}=Link, {AccUris, AccNodes}) ->
					      Id = create_link_id(Link),
					      LinkNode = occi_node:new(Link#occi_link{id=Id, source=ResId}, Owner),
					      { AccUris, [ LinkNode | AccNodes ]}
				      end, {sets:new(), []}, occi_resource:get_links(Res)),
    { Node#occi_node{data=Res#occi_resource{links=Links}},
      LinksNodes }.

save_resource_links([]) ->
    ok;
save_resource_links([ Node | Nodes ]) ->
    case save(Node) of
	ok ->
	    save_resource_links(Nodes);
	{error, Err} ->
	    {error, Err}
    end.

    
create_link_id(#occi_link{cid=KindId}) ->
    case occi_category_mgr:get(KindId) of
	{ok, #occi_kind{location=#uri{path=Prefix}}} ->
	    occi_config:gen_id(Prefix);
	{error, _} ->
	    throw({error, {invalid_kind, KindId}})
    end.
	    
