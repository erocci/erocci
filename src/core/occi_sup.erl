%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2013 Jean Parpaillon.
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
    TableMgr = {occi_table_mgr,
		{occi_table_mgr, start_link, []},
		permanent,
		infinity,
		worker,[occi_table_mgr]},
    Config = {occi_config,
	     {occi_config, start_link, []},
	     permanent,
	     infinity,
	     supervisor,
	     [occi_config]},
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
    Hook = {occi_hook,
	    {occi_hook, start_link, []},
	    permanent,
	    infinity,
	    worker,
	    [occi_hook]},
    Children = [TableMgr, Config, Store, Listener, Hook],
    {ok, {{one_for_one, 10, 10}, Children}}.
