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
-export([register_extension/2,
	 register_category/3,
	 register_action/1,
	 get_categories/0,
	 get_actions/0]).

%% Supervisor callbacks
-export([init/1, get_ref/1]).

-define(SERVER, ?MODULE).

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

register_extension({xml, Path}, Mapping) ->
    case occi_parser_xml:load_extension(Path) of
	{error, parse_error} ->
	    {error, parse_error};
	Ext ->
	    lists:foreach(fun(Cat) -> 
				  Id = occi_category:get_id(Cat),
				  case catch dict:find(Id, Mapping) of
				      error ->
					  lager:error("Unmapped category: ~s~s~n", [Id#occi_cid.scheme, 
										    Id#occi_cid.term]),
					  throw({unmapped_category, Id});
				      {ok, Uri} -> 				  
					  register_category(Id, Cat, Uri)
				  end
			  end,
			  occi_extension:get_categories(Ext))
    end.

register_category(Id, Cat, Uri) ->
    Id = Cat#occi_category.id,
    lager:info("Registering ~s: ~s~s -> ~s~n", 
	       [ Id#occi_cid.class, Id#occi_cid.scheme, Id#occi_cid.term, Uri ]),
    mnesia:transaction(fun() ->
			       mnesia:write(Cat#occi_category{location=Uri})
		       end),
    lists:foreach(fun(Action) ->
			  register_action(Action)
		  end,
		  occi_category:get_actions(Cat)).

register_action(Action) ->
    Id = Action#occi_action.id,
    lager:info("Registering action: ~s~s~n", 
	       [ Id#occi_cid.scheme, Id#occi_cid.term ]),
    mnesia:transaction(fun() ->
			       mnesia:write(Action)
		       end).    

-spec get_categories() -> [occi_category()].
get_categories() ->
    mnesia:dirty_match_object(#occi_category{_ ='_'}).

-spec get_actions() -> [occi_action()].
get_actions() ->
    mnesia:dirty_match_object(#occi_action{_ ='_'}).

-spec get_ref(occi_cid()) -> reference().
get_ref(#occi_cid{}=Id) ->
    case mnesia:dirty_match_read(category_entry, Id) of
	[] ->
	    undefined;
	[Entry] ->
	    Entry#occi_category.ref
    end.

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
    mnesia:create_table(occi_category,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, occi_category)}]),
    mnesia:create_table(occi_action,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, occi_action)}]),
    mnesia:wait_for_tables([category_entry, action_entry], 
			   infinite),
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
