%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_store).
-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).

-include_lib("stdlib/include/qlc.hrl").
-include("occi.hrl").

%% API
-export([start_link/0,
	 save/1,
	 get/2,
	 find/2,
	 update/1,
	 delete/2]).
-export([get_categories/0]).
-export([start_backends/0, 
	 parse_backends/1,
	 parse_categories/1,
	 validate_backends/1,
	 validate_store/1,
	 validate_categories/1]).

%% supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

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

-spec save(occi_entity()) -> {ok, occi_entity()}.
save(Entity) ->
    CatId = occi_tools:get_category_id(Entity),
    Backend = get_backend(CatId),
    gen_server:call(Backend, {save, Entity}).

-spec get(occi_category_id(), occi_entity_id()) -> {ok, occi_entity()}.
get(CatId, Id) ->
    Backend = get_backend(CatId),
    gen_server:call(Backend, {get, CatId, Id}).

-spec find(occi_category_id(), occi_filter()) -> {ok, [occi_entity()]}.
find(CatId, Filter) ->
    Backend = get_backend(CatId),
    gen_server:call(Backend, {find, CatId, Filter}).

-spec update(occi_entity()) -> ok.
update(Entity) ->
    CatId = occi_tools:get_category_id(Entity),
    Backend = get_backend(CatId),
    gen_server:call(Backend, {update, CatId, Entity}).

-spec delete(occi_category_id(), occi_entity_id()) -> ok.
delete(CatId, Id) ->
    Backend = get_backend(CatId),
    gen_server:call(Backend, {delete, CatId, Id}).

-spec get_categories() -> [occi_type()].
get_categories() ->
    mnesia:dirty_match_object({occi_type, '_', '_', '_'}).

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
-spec get_backend(occi_category_id()) -> pid().
get_backend(CatId) ->
    case mnesia:dirty_read(occi_type, CatId) of
	[ Backend ] -> Backend;
	[] ->
	    lager:error("No backend associated to this category: ~p ~n", [CatId]),
	    error
    end.

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
	      fun({Ref, Scheme, Term}) ->
		      case sets:is_element(Ref, Backends) of
			  false ->
			      lager:error("Unknown backend: ~s~n", [Ref]),
			      throw({error, einval});
			  true ->
			      register_categories2(Ref, Scheme, Term)
		      end
	      end, Ls)
    end.

register_categories2(Backend, Scheme, any) ->
    F = fun({Mod, _Scheme, _Term}) -> 
		case occi_renderer:get_id(Mod) of
		    {occi_category_id, Scheme, _} -> true;
		    {occi_category_id,_, _} -> false
		end
	end,
    register_categories3(Backend, F);
register_categories2(Backend, Scheme, Term) ->
    F = fun({Mod, _Scheme, _Term}) -> 
		case occi_renderer:get_id(Mod) of
		    {occi_category_id, Scheme, Term} -> true;
		    {occi_category_id, _, _} -> false
		end
	end,
    register_categories3(Backend, F).

register_categories3(Backend, Filter) ->
    Categories = occi_config:get(categories, fun validate_categories/1),
    Filtered = lists:filter(Filter, Categories),
    Trans = fun() -> lists:foreach(fun({Mod, Scheme, Term}) ->
					   Id = #occi_category_id{scheme=Scheme, term=Term},
					   Type = #occi_type{id=Id, module=Mod, backend=Backend},
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

%-spec validate_categories([atom()]) -> [atom(), atom(), atom()].
validate_categories(Opts) ->
    lists:map(fun(Mod) ->
		      case is_module(Mod) of
			  true ->
			      {occi_category_id, Scheme, Term} = occi_renderer:get_id(Mod),
			      {Mod, Scheme, Term};
			  false ->
			      lager:error("~p is not a valid module", [Mod]),
			      throw({error, einval})
		      end
	      end,
	      Opts).

-spec validate_store([{atom(), atom(), atom()}]) -> [{atom(), atom(), atom()}].
validate_store(Opts) ->
    lists:map(fun({Ref, Scheme, Term}) -> 
		      {Ref, Scheme, Term};
		 (Opt) ->
		      lager:error("Invalid value for store option: ~s~n", [Opt]),
		      throw({error, einval})
	      end, Opts).
