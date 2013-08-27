%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  6 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_listener).
-compile({parse_transform, lager_transform}).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_listeners/0, parse_listeners/1]).

-type opts() :: [{atom(), any()}].
-callback start_link(opts()) -> ok | {error, atom()}.
-callback terminate() -> ok.
-callback validate_cfg(opts()) -> opts().

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

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
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

start_listeners() ->
    case occi_config:get(listeners, fun validate_cfg/1) of
	undefined -> 
	    ignore;
	Ls ->
	    Ls2 = lists:map(
		    fun({Module, Opts}) ->
			    case start_listener(Module, Opts) of
				{ok, _Pid} = Res -> Res;
				{error, Error} -> throw(Error)
			    end
		    end, Ls),
	    {ok, Ls2}
    end.

parse_listeners(Listeners) ->
    lists:map(fun(I) -> parse_listener(I) end, Listeners).

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
init(_) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Childs = [],

    {ok, {SupFlags, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_listener({Module, Opts}) ->
    % TODO: check
    %   - Module is valid
    {Module, Opts}.

start_listener(Module, Opts) ->
    case start_listener2(Module, Opts) of
	{ok, _Pid} = R -> R;
	{error, {{'EXIT', {undef, [{M, _F, _A}|_]}}}, _} = Error ->
		lager:error("Error starting listener: ~p.~n"
			    "Error: ~p.~n", [Module, Error]),
	    {error, {module_not_available, M}};
	{error, {already_started, Pid}} ->
	    {ok, Pid};
	{error, Error} ->
	    {error, Error}
    end.

start_listener2(Module, Opts) ->
    ChildSpec = {Module,
		 {Module, start_link, [Opts]},
		 permanent,
		 brutal_kill,
		 worker,
		 [Module]},
    supervisor:start_child(?SUPERVISOR, ChildSpec).

validate_cfg(L) ->
    lists:map(fun({Module, Opts}) -> {Module, Module:validate_cfg(Opts)} end, L).
