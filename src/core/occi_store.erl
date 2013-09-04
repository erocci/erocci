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
-export([start_link/0,
	 get_backend/1]).
-export([get_categories/0,
	 is_valid_path/1]).
-export([start_backends/0, 
	 parse_backends/1,
	 parse_categories/1,
	 validate_backends/1,
	 validate_store/1,
	 validate_categories/1]).

%% supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

-type tokens() :: [tokens()].
-record(occi_type, {id        :: occi_cid(), 
		    mod       :: atom(), 
		    backend   :: atom(),
		    uri       :: tokens()}).
-type(occi_type() :: #occi_type{}).
-export_type([occi_type/0]).

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

-spec get_backend(occi_cid()) -> pid().
get_backend(CatId) ->
    case mnesia:dirty_read(occi_type, CatId) of
	[ #occi_type{backend=Backend} ] -> Backend;
	[] ->
	    lager:error("No backend associated to this category: ~p ~n", [CatId]),
	    error
    end.

start_backends() ->
    Backends = case occi_config:get(backends, fun validate_backends/1) of
		   undefined ->
		       lager:error("No backend defined"),
		       throw({error, einval});
		   Ls -> 
		       lists:map(
			 fun({Ref, Module, Opts}) ->
				 case start_backend(Ref, Module, Opts) of
				     {ok, _Pid} -> Ref;
				     {error, Error} -> throw(Error)
				 end
			 end, Ls)
	       end,
    register_categories(sets:from_list(Backends)).

-spec get_categories() -> [occi_category()].
get_categories() ->
    Types = mnesia:dirty_match_object(#occi_type{ _ ='_'}),
    Categories = fun(#occi_type{id=Id, mod=Mod}, Acc) ->
			 case Id#occi_cid.class of
			     kind ->
				 [occi_type:get_kind(Mod) | Acc];
			     mixin ->
				 [occi_type:get_mixin(Mod) | Acc];
			     _ ->
				 Acc
			 end
		 end,
    Actions = fun(#occi_type{mod=Mod}) ->
		      occi_type:get_actions(Mod)
	      end,
    lists:flatten([lists:foldl(Categories, [], Types), lists:map(Actions, Types)]).

-spec is_valid_path(Path :: cowboy_router:tokens()) -> true | false.
is_valid_path(_Path) ->
    true.

parse_backends(Backends) ->
    lists:map(fun(Backend) -> parse_backend(Backend) end, Backends).

parse_categories(Categories) ->
    lists:map(fun(Cat) -> parse_category(Cat) end, Categories).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init([]) ->
    mnesia:create_table(occi_type,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, occi_type)}]),
    % start no child, will be added with create_backend
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% internals
%%%===================================================================
parse_backend({Ref, Module, Opts}) ->
    {Ref, Module, Opts};
parse_backend(O) ->
    lager:error("Error parsing backend definition: ~p~n", [O]),
    throw({error, einval}).

parse_category(Mod) when is_atom(Mod) ->
    Mod;
parse_category(O) ->
    lager:error("Error parsing category mapping: ~p~n", [O]),
    throw({error, einval}).

is_module(Mod) when is_atom(Mod) ->
    try Mod:module_info() of
	_InfoList ->
	    true
    catch
	_:_ ->
	    false
    end;
is_module(_) ->
    false.

-spec start_backend(atom(), atom(), term()) -> {ok, pid()} | ignore | {error, term()}.
start_backend(Ref, Module, Opts) ->
    Backend = {Ref, {occi_backend, start_link, [Ref, Module, Opts]}, 
	       permanent, 5000, worker, [occi_backend, Module]},
    supervisor:start_child(?SUPERVISOR, Backend).

-spec register_categories(set()) -> ok.
register_categories(Backends) ->
    case occi_config:get(store, fun validate_store/1) of
	undefined ->
	    [];
	Ls ->
	    lists:map(
	      fun({Ref, Scheme, Term, Class}) ->
		      case sets:is_element(Ref, Backends) of
			  false ->
			      lager:error("Unknown backend: ~s~n", [Ref]),
			      throw({error, einval});
			  true ->
			      Filter = get_filter(Scheme, Term, Class),
			      register_categories2(Ref, Filter)
		      end
	      end, Ls)
    end.

get_filter('_', '_', '_') ->
    fun(_Mod) -> true end;
get_filter(Scheme, '_', '_') ->
    fun(Mod) -> 
	    case occi_type:get_id(Mod) of
		#occi_cid{scheme=Scheme} -> true;
		_ -> false
	    end
    end;
get_filter(Scheme, '_', Class) ->
    fun(Mod) -> 
	    case occi_type:get_id(Mod) of
		#occi_cid{scheme=Scheme, class=Class} -> true;
		_ -> false
	    end
    end;
get_filter('_', '_', Class) ->
    fun(Mod) -> 
	    case occi_type:get_id(Mod) of
		#occi_cid{class=Class} -> true;
		_ -> false
	    end
    end;
get_filter(Scheme, Term, Class) ->
    fun(Mod) -> 
	    case occi_type:get_id(Mod) of
		#occi_cid{scheme=Scheme, term=Term, class=Class} -> true;
		_ -> false
	    end
    end.

register_categories2(Backend, Filter) ->
    Categories = occi_config:get(categories, fun validate_categories/1),
    Filtered = lists:filter(Filter, Categories),
    Trans = fun() -> lists:foreach(
		       fun(Mod) ->
			       Id = occi_type:get_id(Mod),
			       Type = #occi_type{id=Id, mod=Mod, 
						 backend=Backend, 
						 uri=occi_type:get_uri(Mod)},
			       lager:info("Registering category: ~p~n", [Type]),
			       mnesia:write(Type)
		       end,
		       Filtered)
	    end,
    mnesia:transaction(Trans).

%%%
%%% Configuration validation functions
%%% 
validate_backends(Opts) ->
    lists:map(fun({Ref, Module, ModOpts}) -> 
		      {Ref, Module, Module:validate_cfg(ModOpts)}
	      end,
	      Opts).

%-spec validate_categories([atom()]) -> [atom()].
validate_categories(Opts) ->
    lists:map(fun(Mod) ->
		      case is_module(Mod) of
			  true -> Mod;
			  false ->
			      lager:error("~p is not a valid module", [Mod]),
			      throw({error, einval})
		      end
	      end,
	      Opts).

-spec validate_store([{atom(), atom(), atom(), atom()}]) -> [{atom(), atom(), atom(), atom()}].
validate_store(Opts) ->
    lists:map(fun({Ref, Scheme, Term, Class}) -> 
		      {Ref, Scheme, Term, Class};
		 (Opt) ->
		      lager:error("Invalid value for store option: ~s~n", [Opt]),
		      throw({error, einval})
	      end, Opts).
