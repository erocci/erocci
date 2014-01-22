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
	 save/2,
	 delete/1,
	 get_collection/1,
	 find/1,
	 find/2,
	 get_backend/1, 
	 is_valid_path/1]).

%% supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).
-define(TABLE, ?MODULE).

-type(backend_ref() :: atom()).
-type(backend_mod() :: atom()).
-type(backend_opts() :: term()).
-type(backend_desc() :: {backend_ref(), backend_mod(), backend_opts()}).

-export_type([backend_ref/0]).

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

-spec register(backend_desc()) ->
		      {ok, pid()} 
			  | ignore 
			  | {error, term()}.
register({Ref, Mod, Opts}) ->
    lager:info("Registering backend: ~p~n", [Ref]),
    Backend = {Ref, {occi_backend, start_link, [Ref, Mod, [{ref, Ref} | Opts]]}, 
	       permanent, 5000, worker, [occi_backend, Mod]},
    case supervisor:start_child(occi_store, Backend) of
	{ok, Pid} ->
	    ets:insert(?TABLE, {backend, Ref}),
	    occi_listener:notify({add_backend, Ref}),
	    {ok, Pid};
	{error, Err} ->
	    {error, Err}
    end.

-spec is_valid_path(Path :: uri()) -> 
			   false 
			       | {entity, atom(), term()}
			       | {category, atom()}.
is_valid_path(Path) ->
    lager:debug("Looking up path: ~p~n", [Path]),
    false.

-spec get_backend(uri()) -> backend_ref().
get_backend(_) ->
    % TODO: allow multiple backends
    case ets:lookup(?TABLE, backend) of
	[] ->
	    undefined;
	[{backend, Ref}] ->
	    Ref
    end.

-spec save(occi_object()) -> ok | {error, term()}.
save(Object) ->
    case get_backend([]) of
	undefined ->
	    {error, undefined_backend};
	Backend ->
	    occi_backend:save(Backend, Object)
    end.

-spec save(backend_ref(), occi_object()) -> ok | {error, term()}.
save(Backend, Object) ->
    occi_backend:save(Backend, Object).

-spec delete(occi_object()) -> ok | {error, term()}.
delete(Object) ->
    case get_backend([]) of
	undefined ->
	    {error, undefined_backend};
	Backend ->
	    occi_backend:delete(Backend, Object)
    end.			        

get_collection(#occi_kind{id=Id, backend=Backend}) ->
    lager:debug("Retrieve collection: ~p (backend ~p)~n", [Id, Backend]),
    case occi_backend:find(Backend, #occi_collection{cid=Id, _='_'}) of
	{ok, [Coll]} ->
	    {ok, Coll};
	_ ->
	    {ok, occi_collection:new(Id)}
    end;
get_collection(#occi_mixin{id=Id, backend=Backend}) ->
    lager:debug("Retrieve collection: ~p (backend ~p)~n", [Id, Backend]),
    case occi_backend:find(Backend, #occi_collection{cid=Id, _='_'}) of
	{ok, [Coll]} ->
	    {ok, Coll};
	_ ->
	    {ok, occi_collection:new(Id)}
    end.

find(Request) ->
    % TODO: fix multiple backends
    case get_backend([]) of
	undefined ->
	    {ok, []};
	Backend ->
	    find(Backend, Request)
    end.

find(Backend, Request) ->
    lager:debug("Find request: ~p~n", [Request]),
    occi_backend:find(Backend, Request).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init([]) ->
    lager:info("Starting OCCI storage manager"),
    ?TABLE = ets:new(?TABLE, [set, public, {keypos, 1}, named_table]),
    % start no child, will be added with backends
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% internals
%%%===================================================================
