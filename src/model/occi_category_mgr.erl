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
%%% Created : 27 Sep 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_category_mgr).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([register/3,
	 get_entries/0,
	 get_all/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-record(category_entry, {id     :: #occi_cid{}, 
			 ref    :: reference(),
			 uri    :: [binary()]}).
-type(category_entry() :: #category_entry{}).
-export_type([category_entry/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec register(term(), uri() | binary(), [occi_hook:hook()]) -> ok.
register(CategoryDef, Location, Hooks) when is_binary(Location) ->
    register(CategoryDef, occi_types:split_path(Location), Hooks);
register(CategoryDef, Location, Hooks) ->
    {Id, Ref} = occi_category:new(CategoryDef, Location),
    Path = occi_types:join_path([<<"">> | Location]),
    lager:info("Registering category: ~s~s (~s) -> ~s~n", 
	       [Id#occi_cid.scheme, Id#occi_cid.term, Id#occi_cid.class, Path]),
    mnesia:transaction(fun() -> 
			       Entry = #category_entry{id=Id, ref=Ref, uri=Location},
			       mnesia:write(Entry)
		       end),
    lists:foreach(fun(Hook) ->
			  occi_hook:add_hook(Id, Hook)
		  end, Hooks).

-spec get_entries() -> [category_entry()].
get_entries() ->
    mnesia:dirty_match_object(#category_entry{_ ='_'}).

-spec get_all() -> [occi_category()].
get_all() ->
    Entries = mnesia:dirty_match_object(#category_entry{_ ='_'}),
    lists:flatten([ [ occi_category:get_obj(Ref) |
		      occi_category:get_attr(Ref, actions) ]
		    || #category_entry{ref=Ref} <- Entries ]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    lager:info("Starting OCCI categories manager"),
    mnesia:create_table(category_entry,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, category_entry)}]),
    mnesia:wait_for_tables([tbl_category], 
			   infinite),
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
