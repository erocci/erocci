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
-export([find/1,
	 find_all/0,
	 get/1,
	 register_extension/2,
	 register_kind/1,
	 register_mixin/1,
	 register_action/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CAT_TBL, ?MODULE).
-define(TYPE_TBL, type_tbl).

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
	    lists:foreach(fun(#occi_kind{id=Id}=Kind) ->
				  case get_uri(Id, Mapping) of
				      none ->
					  lager:info("Ignore category: ~p~n", [lager:pr(Id, ?MODULE)]);
				      #uri{}=Uri ->
					  register_kind(Kind#occi_kind{location=Uri})
				  end;
			     (#occi_mixin{id=Id}=Mixin) ->
				  case get_uri(Id, Mapping) of
				      none ->
					  lager:info("Ignore category: ~p~n", [lager:pr(Id, ?MODULE)]);
				      #uri{}=Uri ->
					  register_mixin(Mixin#occi_mixin{location=Uri})
				  end
			  end,
			  occi_extension:get_categories(Ext)),
	    lists:foreach(fun(#occi_type{}=Type) ->
				  register_type(Type)
			  end, occi_extension:get_types(Ext))
    end.

register_kind(#occi_kind{id=Id, location=#uri{}=Uri}=Kind) ->
    lager:info("Registering kind: ~p -> ~p~n", [ lager:pr(Id, ?MODULE), lager:pr(Uri, ?MODULE) ]),
    ets:insert(?CAT_TBL, Kind),
    lists:foreach(fun(Action) ->
			  register_action(Action)
		  end,
		  occi_kind:get_actions(Kind)).

register_mixin(#occi_mixin{id=Id, location=Uri}=Mixin) ->
    lager:info("Registering mixin: ~p -> ~p~n", [ lager:pr(Id, ?MODULE), lager:pr(Uri, ?MODULE) ]),
    ets:insert(?CAT_TBL, Mixin),
    lists:foreach(fun(Action) ->
			  register_action(Action)
		  end,
		  occi_mixin:get_actions(Mixin)).

register_action(#occi_action{id=Id}=Action) ->
    lager:info("Registering action: ~p~n", [ lager:pr(Id, ?MODULE) ]),
    ets:insert(?CAT_TBL, Action).

register_type(#occi_type{id=Id}=Type) ->
    lager:info("Registering type: ~p~n", [ Id ]),
    ets:insert(?TYPE_TBL, Type).

-spec find(occi_category() | uri()) -> [occi_category()].
find(#uri{path=Path}) ->
    case ets:match_object(?CAT_TBL, #occi_kind{location=#uri{path=Path, _='_'}, _='_'}) of
	[] ->
	    ets:match_object(?CAT_TBL, #occi_mixin{location=#uri{path=Path, _='_'}, _='_'});
	Other ->
	    Other
    end;

find(#occi_cid{}=Cid) ->
    case ets:match_object(?CAT_TBL, #occi_kind{id=Cid, _='_'}) of
	[] ->
	    ets:match_object(?CAT_TBL, #occi_mixin{id=Cid, _='_'});
	Res ->
	    Res
    end;
	    	    
find(#occi_kind{}=Kind) ->
    ets:match_object(?CAT_TBL, Kind);

find(#occi_mixin{}=Mixin) ->
    ets:match_object(?CAT_TBL, Mixin);

find(#occi_action{}=Action) ->
    ets:match_object(?CAT_TBL, Action).

-spec find_all() -> {[occi_kind()], [occi_mixin()], [occi_action()]}.
find_all() ->
    { find(#occi_kind{_='_'}),
      find(#occi_mixin{_='_'}),
      find(#occi_action{_='_'}) }.

-spec get(occi_cid() | occi_type()) -> occi_mixin().
get(#occi_cid{class=mixin}=Cid) ->
    case ets:match_object(?CAT_TBL, #occi_mixin{id=Cid, _='_'}) of
	[Mixin] ->
	    Mixin;
	_ ->
	    throw({error, unknown_category})
    end;

get(#occi_cid{class=kind}=Cid) ->
    case ets:match_object(?CAT_TBL, #occi_kind{id=Cid, _='_'}) of
	[Kind] ->
	    Kind;
	_ ->
	    throw({error, unknown_category})
    end;

get(#occi_cid{class=action}=Cid) ->
    case ets:match_object(?CAT_TBL, #occi_action{id=Cid, _='_'}) of
	[Action] ->
	    Action;
	_ ->
	    throw({error, unknown_action})
    end;

get(#occi_type{id=Id}) ->
    case ets:lookup(?TYPE_TBL, Id) of
	[Type] ->
	    Type;
	_ ->
	    throw({error, {unknown_type, Id}})
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
    ?CAT_TBL = ets:new(?CAT_TBL, 
		     [ordered_set, public, {keypos, 2}, named_table, {read_concurrency, true}]),
    ?TYPE_TBL = ets:new(?TYPE_TBL, 
			[ordered_set, public, {keypos, 2}, named_table, {read_concurrency, true}]),
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_uri(_Id, []) ->
    none;
get_uri(Id, [{Id, Uri}|_Tail]) ->
    occi_uri:parse(Uri);
get_uri(Id, [_H|Tail]) ->
    get_uri(Id, Tail).
