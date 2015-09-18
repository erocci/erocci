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
%%%
%%%      occi_store can return occi_store_err
%%%         - 500 : a internal server error happened
%%%         - 403 : the request is understood but the user is not allowed to execute this request
%%%         - 400 : bad request
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_store).

-include("occi.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, register/1]).
-export([get/1]).
-export([save/1,
         save/2,
         update/2,
         delete/2,
         find/1,
         load/1,
         load/3,
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
-type(occi_store_err() :: 500 | 403 | 400).

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
    ?info("Registering backend: ~p~n", [Ref]),
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
			?error("Error starting backend: ~p", [Err]),
            {error, Err}
    end.

-spec save(occi_node()) -> ok | {error, occi_store_err()}.
save(#occi_node{type=occi_collection, objid=#occi_cid{}}=Node) ->
    ?debug("occi_store:save(~p)~n", [Node]),
    cast(save, filter_collection(Node)).

-spec save(occi_node(), occi_store_ctx()) -> ok | {error, occi_store_err()}.
save(#occi_node{id=#uri{path=Path}, owner=User}=Node, #occi_store_ctx{user=_User_ctx, auth_ref=RefU}) ->
    case occi_acl:check(create, Node, User, RefU) of
        allow ->    ?debug("occi_store:save(~p)~n", [Node]),
                    case get_backend(Path) of
                        {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
                            case occi_backend:save(Ref, occi_node:rm_prefix(Node, Prefix)) of
                                ok -> ok;
                                {error, _Err} -> 
                                    {error, 400}
                            end;
                        {error, _Err} ->
                            {error, 500}
                    end;
        deny -> {error, 403}
    end.


-spec update(occi_node(), occi_store_ctx()) -> ok | {error, occi_store_err()}.
update(#occi_node{id=#uri{path=Path}, type=capabilities, data=#occi_mixin{}=Mixin}=Node, #occi_store_ctx{user=User_ctx, auth_ref=RefU}) ->
    case occi_acl:check(update, capabilities, User_ctx, RefU) of
        allow -> 
            ?debug("occi_store:update(~p)~n", [Node]),
            case get_backend(Path) of
                {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
                    case occi_backend:update(Ref, occi_node:rm_prefix(Node, Prefix)) of
                        ok ->   occi_category_mgr:register_mixin(Mixin),
                                ok;
                        {error, _Err} ->
                            {error, 400}
                    end;
                {error, _Err} ->
                    {error, 500}
            end;
        deny -> {error, 403}
    end;

update(#occi_node{type=occi_collection, objid=#occi_cid{}}=Node, _Ctx) ->
    ?debug("occi_store:update(~p)~n", [Node]),
    cast(update, filter_collection(Node));

update(#occi_node{id=#uri{path=Path}, owner=_User}=Node, #occi_store_ctx{user=User_ctx, auth_ref=RefU}) ->
    case occi_acl:check(update, Node, User_ctx, RefU) of
        allow ->?debug("occi_store:update(~p)~n", [Node]),
                case get_backend(Path) of
                    {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
                        case occi_backend:update(Ref, occi_node:rm_prefix(Node, Prefix)) of
                            ok ->   ok;
                            {error, _Err} ->
                                {error, 400}
                        end;
                    {error, _Err} ->
                        {error, 500}
                end;
        deny -> {error, 403}
    end.

-spec delete(occi_node(), occi_store_ctx()) -> ok | {error, occi_store_err()}.
delete(#occi_node{data=undefined, owner=User}=Node, #occi_store_ctx{user=_User_ctx, auth_ref=RefU}=Ctx) ->
    case occi_acl:check(delete, Node, User, RefU) of
        allow ->case load(Node, #occi_store_opts{}, Ctx) of
                    {ok, Node2} -> delete(Node2);
                    {error, Err} -> {error, Err}
                end;
        deny -> {error, 403}
    end.

-spec get(occi_cid()) -> {ok, occi_category()} | {error, term()}.
get(Cid) ->
    occi_category_mgr:get(Cid).

-spec find(occi_node() | occi_cid()) -> {ok, [occi_node()]} | {error, term()}.
find(#occi_node{type=capabilities, objid=#occi_cid{}=Cid}=Req) ->
    ?debug("occi_store:find(~p)~n", [Req]),
    case occi_category_mgr:find(Cid) of
        [#occi_mixin{location=#uri{path=Path}}] ->
            case get_backend(Path) of
                {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
                    case occi_backend:find(Ref, Req) of
                        {ok, [#occi_node{}=Node]} ->
                            {ok, [occi_node:add_prefix(Node, Prefix)]};
                        {ok, []} ->
                            {ok, []};
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
    ?debug("occi_store:find(~p)~n", [Req]),
    {K, M, A} = occi_category_mgr:find_all(),
    {ok, [occi_capabilities:new(K, M, A)]};

find(#occi_node{type=occi_collection, objid=#occi_cid{}=Cid}=Req) ->
    ?debug("occi_store:find(~p, ~p)~n", [Req]),
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

find(#occi_node{id=undefined}) ->
    {ok, []};

find(#occi_node{id=#uri{path=Path}=Id}=Req) ->
    ?debug("occi_store:find(~p)~n", [Req]),
    case occi_category_mgr:find(Id) of
        [] ->
            case get_backend(Path) of
                {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
                    case occi_backend:find(Ref, occi_node:rm_prefix(Req, Prefix)) of
                        {ok, []} -> {ok, []};
                        {ok, [Node]} ->
                            {ok, [occi_node:add_prefix(Node, Prefix)]};
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


-spec load(occi_node()) -> {ok, occi_node()} | {error, occi_store_err()}.
load(Req) ->
    load(Req, #occi_store_opts{}, anonymous).

-spec load(occi_node(), occi_store_opts(), occi_store_ctx()) -> {ok, occi_node()} | {error, occi_store_err()}.
load(#occi_node{type=capabilities, data=undefined}=Req, _Opts, _Ctx) ->
    {ok, [Res]} = find(Req),
    {ok, Res};

load(#occi_node{type=occi_collection, data=undefined}=Node, #occi_store_opts{node_f=NF}=Opts, 
     #occi_store_ctx{user=User, auth_ref=AuthRef}) ->
    case occi_acl:check(read, Node, User, AuthRef) of
        allow ->
            AuthFun = fun (N) ->
                               case occi_acl:check(read, N, User, AuthRef) of
                                   allow -> true;
                                   deny -> false
                               end
                      end,
            load_collection(Node, Opts#occi_store_opts{node_f=[AuthFun | NF]});
        deny -> {error, 403}
    end;

load(#occi_node{id=#uri{path=Path}, data=undefined}=Node, Opts, anonymous) ->
    ?debug("occi_store:load(~p, ~p)~n", [Node, Opts]),
    case get_backend(Path) of
        {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
            case occi_backend:load(Ref, occi_node:rm_prefix(Node, Prefix), Opts) of
                {ok, Node2} ->
                    {ok, occi_node:add_prefix(Node2, Prefix)};
                {error, _Err} ->
                    {error, 400}
            end;
        {error, _Err} ->
            {error, 500}
    end;

load(#occi_node{id=#uri{path=Path}, data=undefined}=Node, Opts, #occi_store_ctx{user=User_ctx, auth_ref=RefU}) ->
    case occi_acl:check(read, Node, User_ctx, RefU) of
        allow ->?debug("occi_store:load(~p, ~p)~n", [Node, Opts]),
                case get_backend(Path) of
                    {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
                        case occi_backend:load(Ref, occi_node:rm_prefix(Node, Prefix), Opts) of
                            {ok, Node2} ->
                                {ok, occi_node:add_prefix(Node2, Prefix)};
                            {error, _Err} ->
                                {error, 400}
                        end;
                    {error, _Err} ->
                        {error, 500}
                end;
        deny -> {error, 403}
    end;

load(#occi_node{data=_}=Node, Opts, _User) ->
    ?debug("occi_store:load(~p, ~p)~n", [Node, Opts]),
    {ok, Node}.

action(#occi_node{type=capabilities}=_N, _A) ->
    ?debug("occi_store:action(~p, ~p)~n", [_N, _A]),
    {error, unsupported_node};

action(#occi_node{type=occi_collection, data=undefined}=Node, A) ->
    ?debug("occi_store:action(~p, ~p)~n", [Node, A]),
    case load(Node) of
        {ok, N2} -> action(N2, A);
        {error, Err} -> {error, Err}
    end;

action(#occi_node{type=occi_collection, data=#occi_collection{entities=E}}=_N, #occi_action{}=A) ->
    ?debug("occi_store:action(~p, ~p)~n", [_N, A]),
    Reqs = ordsets:fold(fun (#uri{path=Path}=Uri, Acc) ->
                                 case get_backend(Path) of
                                     {error, Err} -> throw({error, Err});
                                     {ok, #occi_node{id=#uri{path=Prefix}}=Backend} ->
                                         [{Backend, [occi_uri:rm_prefix(Uri, Prefix), A]} | Acc]
                                 end
                        end, [], E),
    cast(action, Reqs);

action(#occi_node{id=#uri{path=Path}=Id}=_N, #occi_action{}=A) ->
    ?debug("occi_store:action(~p, ~p)~n", [_N, A]),
    case get_backend(Path) of
        {error, Err} -> throw({error, Err});
        {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
            occi_backend:action(Ref, occi_uri:rm_prefix(Id, Prefix), A)
    end.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init([]) ->
    ?info("Starting OCCI storage manager"),
    ?TABLE = ets:new(?TABLE, [set, public, {keypos, 1}, named_table]),
    ets:insert(?TABLE, {tree, gb_trees:empty()}),
    ets:insert(?TABLE, {set, gb_sets:new()}),
    % start no child, will be added with backends
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% internals
%%%===================================================================
delete(#occi_node{type=occi_collection, objid=#occi_cid{}}=Node) ->
    ?debug("occi_store:delete(~p)~n", [Node]),
    cast(delete, filter_collection(Node));

delete(#occi_node{id=#uri{path=Path}, type=capabilities, data=#occi_mixin{}=Mixin}=Node) ->
    ?debug("occi_store:delete(~p)~n", [Node]),
    case get_backend(Path) of
        {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
            case occi_backend:delete(Ref, occi_node:rm_prefix(Node, Prefix)) of
                ok ->   occi_category_mgr:unregister_mixin(Mixin),
                        ok;
                {error, _Err} ->
                    {error, 400}
            end;
        {error, _Err} ->
            {error, 500}
    end;

delete(#occi_node{id=#uri{path=Path}}=Node) ->
    ?debug("occi_store:delete(~p)~n", [Node]),
    case get_backend(Path) of
        {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
            case occi_backend:delete(Ref, occi_node:rm_prefix(Node, Prefix)) of
                ok ->   ok;
                {error, _Err} ->
                    {error, 400}
            end;
        {error, _Err} ->
            {error, 500}
    end;

delete(_Node) ->
    ?debug("occi_store:delete(~p)~n", [_Node]),
    {error, 500}.

load_collection(#occi_node{objid=#occi_cid{}=Cid, type=occi_collection, data=undefined}=Node, Opts) ->
    ?debug("occi_store:load(~p, ~p)~n", [Node, Opts]),
    Merge = fun (_B, #occi_node{data=undefined}, Acc) ->
                     Acc;
               (#occi_node{id=#uri{path=Prefix}}, #occi_node{data=C}, Acc) ->
                    occi_collection:merge(Acc, occi_collection:add_prefix(C, Prefix))
            end,
    case fold(Merge, occi_collection:new(Cid), load, [Node, Opts]) of
        {ok, Coll} ->
            {ok, Node#occi_node{data=Coll}};
        {error, Err} ->
            {error, Err}
    end;

load_collection(#occi_node{id=#uri{path=Path}, data=undefined}=Node, Opts) ->
    ?debug("occi_store:load(~p, ~p)~n", [Node, Opts]),
    case get_backend(Path) of
        {ok, #occi_node{id=#uri{path=Prefix}, objid=Ref}} ->
            case occi_backend:load(Ref, occi_node:rm_prefix(Node, Prefix), Opts) of
                {ok, Node2} ->
                    {ok, occi_node:add_prefix(Node2, Prefix)};   
                {error, Err} ->
                    ?debug("Error ~p~n", [Err]),
                    {error, 500}
            end;
        {error, _Err} ->
            {error, 500}
    end.


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

cast(_, []) ->
    ok;

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
%%% return list of {mountpoint, [occi_node{type=occi_collection}]}
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
                                 gb_trees:insert(Backend, [Node#occi_node{data=C}], Acc);
                             {value, [#occi_node{data=#occi_collection{}=C}]} ->
                                 C2 = occi_collection:add_entity(C, occi_uri:rm_prefix(Uri, Prefix)),
                                 gb_trees:update(Backend, [Node#occi_node{data=C2}], Acc)
                         end
                 end
        end,
    gb_trees:to_list(ordsets:fold(F, gb_trees:empty(), E)).

load_user_mixins(#occi_node{objid=Ref}) ->
    case occi_backend:find(Ref, #occi_node{id=#uri{path="/-/"}, type=capabilities, _='_'}) of
        {ok, [#occi_node{data={_, Mixins, _}}]} ->
            lists:foreach(fun (Mixin) -> 
                                   occi_category_mgr:register_mixin(Mixin)
                          end, Mixins);
        {error, Err} ->
            {error, Err}
    end.
