%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2013 Jean Parpaillon.

%% @doc Supervisor for the occi core application.

-module(occi_sup).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%% ChildSpec = {Id, StartFunc, Restart, Shutdown, Type, Modules}
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init(_) ->
    Store = {occi_store,
	     {occi_store, start_link, []},
	     permanent,
	     infinity,
	     supervisor,
	     [occi_store]},
    Listener = {occi_listener,
		{occi_listener, start_link, []},
		permanent,
		infinity,
		supervisor,
		[occi_listener]},
    Children = [Store, Listener],
    {ok, {{one_for_one, 10, 10}, Children}}.
