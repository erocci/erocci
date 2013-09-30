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
-export([start_link/0, register/2]).
-export([get_backend/1, is_valid_path/1]).

%% supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

-record(store_mountpoint, {uri :: [binary()], backend :: backend_ref()}).
-type(store_mountpoint() :: #store_mountpoint{}).

-type(backend_ref() :: atom()).
-type(backend_mod() :: atom()).
-type(backend_opts() :: term()).
-type(backend_desc() :: {backend_ref(), backend_mod(), backend_opts()}).

-export_type([store_mountpoint/0]).

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

-spec register(backend_desc(), uri()) ->
		 {ok, pid()} 
		     | ignore 
		     | {error, term()}.
register({Ref, Mod, Opts}, Path) ->
    lager:info("Registering backend: ~p -> ~p~n", [Ref, Path]),
    register_mountpoint(Path, Ref),
    Backend = {Ref, {occi_backend, start_link, [Ref, Mod, [{ref, Ref} | Opts]]}, 
	       permanent, 5000, worker, [occi_backend, Mod]},
    supervisor:start_child(occi_store, Backend).

-spec is_valid_path(Path :: uri()) -> 
			   false 
			       | {entity, atom(), term()}
			       | {category, atom()}.
is_valid_path(Path) ->
    lager:debug("Looking up path: ~p~n", [Path]),
    false.

-spec get_backend(uri()) -> backend_ref().
get_backend(Path) ->
    get_backend2(lists:reverse(Path)).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init([]) ->
    lager:info("Starting OCCI storage manager"),
    mnesia:create_table(store_mountpoint,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, store_mountpoint)}]),
    mnesia:wait_for_tables([store_mountpoint], 
			   infinite),
    % start no child, will be added with backends
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% internals
%%%===================================================================

%% @doc Store path as reverse ordered tokens: optimized for lookup
%%
register_mountpoint(Path, Ref) ->
    Trans = fun() -> 
		    Mp = #store_mountpoint{uri=occi_types:split_path(Path), backend=Ref},
		    mnesia:write(Mp)
	    end,
    mnesia:transaction(Trans).

get_backend2([]) ->
    [Mp] = mnesia:dirty_read(store_mountpoint, []),
    Mp#store_mountpoint.backend;
get_backend2([H|T]) ->
    case mnesia:dirty_read(store_mountpoint, [H|T]) of
	[] ->
	    get_backend2(T);
	[Mp] ->
	    Mp#store_mountpoint.backend
    end.
