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
-export([create/1,
	 create/2,
	 get_collection/1,
	 find/1,
	 gen_id/2,
	 get_backend/0, 
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

-spec get_backend() -> backend_ref().
get_backend() ->
    [{backend, Ref}] = ets:lookup(?TABLE, backend),
    Ref.

-spec gen_id(binary(), binary()) -> binary().
gen_id(Host, Prefix) when is_binary(Prefix), 
			  is_binary(Host) ->
    Id = list_to_binary(uuid:to_string(uuid:uuid3(uuid:uuid4(), Host))),
    <<"http://", Host/binary, Prefix/binary, Id/binary>>.

-spec create(occi_object()) -> {ok, occi_object()} | {error, term()}.
create(Obj) ->
    Backend = get_backend(),
    create(Backend, Obj).

-spec create(backend_ref(), occi_object()) -> {ok, occi_resource()} 
						  | {error, term()}.
create(Backend, #occi_resource{id=Id}=Res) ->
    lager:debug("Create resource: ~s~n", [Id]),
    occi_backend:save(Backend, Res);
create(Backend, #occi_mixin{id=Id}=Mixin) ->
    lager:debug("Create mixin: ~s~s~n", [Id#occi_cid.scheme, Id#occi_cid.term]),
    occi_backend:save(Backend, Mixin).

get_collection(#occi_kind{id=Id, backend=Backend}) ->
    lager:debug("Retrieve collection: ~p~n", [Id]),
    occi_backend:find_all(Backend, Id);
get_collection(#occi_mixin{id=Id, backend=Backend}) ->
    lager:debug("Retrieve collection: ~p~n", [Id]),
    occi_backend:find_all(Backend, Id).

find(Request) ->
    Backend = get_backend(),
    find(Backend, Request).

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
