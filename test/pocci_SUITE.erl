%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2015-2016 Jean Parpaillon
%%% @doc Execute tests from pOCCI: https://github.com/CESNET/pOCCI
%%%
%%% @end
%%% Created :  3 Dec 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(pocci_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(PORT, 9999).
-define(ENDPOINT, "http://localhost:9999").

suite() ->
	[{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    application:load(erocci_core),

    application:set_env(erocci_core, listeners, 
                        [ {http, erocci_http, [{port, ?PORT}]} ]
					   ),
    Schemas = {schema, get_data_path("occi-infrastructure.xml", Config)},
    application:set_env(erocci_core, backends,
                        [{mnesia, erocci_backend_mnesia, [Schemas], <<"/">>}]),
    application:set_env(erocci_core, acl,
                        [{allow, '_', '_', '_'}]),
	{ok, Cwd} = file:get_cwd(),

	application:load(mnesia),
	application:set_env(mnesia, dir, filename:join(Cwd, "mnesia")),
	mnesia:create_schema([node()]),

    application:ensure_all_started(erocci_core),
    pocci_config(Config).

end_per_suite(_Config) ->
    error_logger:delete_report_handler(cth_log_redirect),
    application:stop(erocci_core),
    error_logger:add_report_handler(cth_log_redirect),
	ok.

all() ->
	[
	 'OCCI_CORE_DISCOVERY_001'
	,'OCCI_CORE_DISCOVERY_002'
	,'OCCI_CORE_CREATE_001'
	,'OCCI_CORE_CREATE_006'
	,'OCCI_CORE_READ_001'
	,'OCCI_CORE_READ_002'
	,'OCCI_CORE_READ_007'
	,'OCCI_CORE_DELETE_001'
	,'OCCI_CORE_UPDATE_001'
	,'OCCI_CORE_MISC_001'

	,'OCCI_INFRA_CREATE_001'
	,'OCCI_INFRA_CREATE_002'
	,'OCCI_INFRA_CREATE_003'
	,'OCCI_INFRA_CREATE_004'
	,'OCCI_INFRA_CREATE_005'
	,'OCCI_INFRA_CREATE_006'
	,'OCCI_INFRA_CREATE_007'
	].



'OCCI_CORE_DISCOVERY_001'(Config) ->
	Cmd = pocci("OCCI/CORE/DISCOVERY/001", Config),
	?assertCmdOutput("OCCI/CORE/DISCOVERY/001  OK\n", Cmd).


'OCCI_CORE_DISCOVERY_002'(Config) ->
	Cmd = pocci("OCCI/CORE/DISCOVERY/002", Config),
	?assertCmdOutput("OCCI/CORE/DISCOVERY/002  OK\n", Cmd).


'OCCI_CORE_READ_001'(Config) ->
	Cmd = pocci("OCCI/CORE/READ/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK\n", Cmd).


'OCCI_CORE_READ_002'(Config) ->
	Cmd = pocci("OCCI/CORE/READ/002", Config),
	?assertCmdOutput("OCCI/CORE/READ/002  OK\n", Cmd).


'OCCI_CORE_READ_007'(Config) ->
	Cmd = pocci("OCCI/CORE/READ/007", Config),
	?assertCmdOutput("OCCI/CORE/READ/007  OK\n", Cmd).


'OCCI_CORE_CREATE_001'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/CREATE/001  OK\n", Cmd).


'OCCI_CORE_CREATE_006'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/006", Config),
	?assertCmdOutput("OCCI/CORE/CREATE/006  OK\n", Cmd).


'OCCI_CORE_DELETE_001'(Config) ->
	Cmd = pocci("OCCI/CORE/DELETE/001", Config),
	?assertCmdOutput("OCCI/CORE/DELETE/001  OK\n", Cmd).


'OCCI_CORE_UPDATE_001'(Config) ->
	Cmd = pocci("OCCI/CORE/UPDATE/001", Config),
	?assertCmdOutput("OCCI/CORE/UPDATE/001  OK\n", Cmd).


'OCCI_CORE_MISC_001'(Config) ->
	Cmd = pocci("OCCI/CORE/MISC/001", Config),
	?assertCmdOutput("OCCI/CORE/MISC/001  OK\n", Cmd).


'OCCI_INFRA_CREATE_001'(Config) ->
	Cmd = pocci("OCCI/INFRA/CREATE/001", Config),
	?assertCmdOutput("OCCI/INFRA/CREATE/001  OK\n", Cmd).


'OCCI_INFRA_CREATE_002'(Config) ->
	Cmd = pocci("OCCI/INFRA/CREATE/002", Config),
	?assertCmdOutput("OCCI/INFRA/CREATE/002  OK\n", Cmd).


'OCCI_INFRA_CREATE_003'(Config) ->
	Cmd = pocci("OCCI/INFRA/CREATE/003", Config),
	?assertCmdOutput("OCCI/INFRA/CREATE/003  OK\n", Cmd).


'OCCI_INFRA_CREATE_004'(Config) ->
	Cmd = pocci("OCCI/INFRA/CREATE/004", Config),
	?assertCmdOutput("OCCI/INFRA/CREATE/004  OK\n", Cmd).


'OCCI_INFRA_CREATE_005'(Config) ->
	Cmd = pocci("OCCI/INFRA/CREATE/005", Config),
	Out = string:tokens(os:cmd(Cmd), "\n"),
	?assertMatch([_, _, "OCCI/INFRA/CREATE/005  OK"], Out).


'OCCI_INFRA_CREATE_006'(Config) ->
	Cmd = pocci("OCCI/INFRA/CREATE/006", Config),
	Out = string:tokens(os:cmd(Cmd), "\n"),
	?assertMatch(["OCCI/INFRA/CREATE/006  OK"], Out).


'OCCI_INFRA_CREATE_007'(Config) ->
	Cmd = pocci("OCCI/INFRA/CREATE/007", Config),
	Out = string:tokens(os:cmd(Cmd), "\n"),
	?assertMatch(["OCCI/INFRA/CREATE/007  OK"], Out).

%%%
%%% Priv
%%%
get_data_path(Path, Config) ->
    DataDir = ?config(data_dir, Config),
    filename:join([DataDir, Path]).

pocci(Name, Config) ->
	Cmd = ?config(pocci, Config) 
		++ " --url " ++ ?ENDPOINT
		++ " --mime-type 'text/plain'"
		++ " --format plain"
		++ " --tests " ++ Name,
	ct:log(info, "cmd = ~s", [Cmd]),
	Cmd.

pocci_config(Config) ->
	PocciConfig = filename:join([?config(data_dir, Config), "pocci.conf"]),
	{ok, [{'POCCI', Path}]} = file:consult(PocciConfig),
	[ {pocci, Path} | Config ].
