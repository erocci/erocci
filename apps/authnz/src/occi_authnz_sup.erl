-module(occi_authnz_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 add_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_child(Mod, Opts) ->
    RealMod = list_to_atom("occi_authnz_mod_" ++ atom_to_list(Mod)),
    case is_module(RealMod) of
	true -> 
	    error_logger:info_msg("Starting authnz backend: ~p~n", [Mod]),
	    Args = [{local, RealMod}, occi_authnz, {RealMod, Opts}, []],
	    Child = {Mod, {gen_server, start_link, Args}, permanent, 5000, worker, [occi_authnz, RealMod]},
	    case supervisor:start_child(?MODULE, Child) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid};
		{error, Err} -> {error, Err}
	    end;
	false ->
	    {error, {invalid_module, Mod}}
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

%%%
%%% Priv
%%%
is_module(Mod) when is_atom(Mod) ->
    try Mod:module_info() of
        _ -> true
    catch _:_ -> false
    end;
is_module(_) -> 
    false.
