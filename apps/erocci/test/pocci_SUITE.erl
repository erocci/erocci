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
-define(ENDPOINT, "https://localhost:9999").

suite() ->
	[{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    application:load(erocci_core),

	Cacertfile = filename:join([?config(data_dir, Config), "ssl", "cowboy-ca.crt"]),
	Certfile = filename:join([?config(data_dir, Config), "ssl", "server.crt"]),
	Keyfile = filename:join([?config(data_dir, Config), "ssl", "server.key"]),
    application:set_env(erocci_core, listeners, 
                        [{https, occi_https, 
						  [{port, ?PORT}, {cacertfile, Cacertfile}, {certfile, Certfile}, {keyfile, Keyfile}]}
						]),
    Schemas = {schemas, [{path, get_data_path("occi-infrastructure.xml", Config)}]},
    application:set_env(erocci_core, backends,
                        [{mnesia, occi_backend_mnesia, [Schemas], <<"/">>}]),
    application:set_env(erocci_core, acl,
                        [{allow, '_', '_', '_'}]),

    application:ensure_all_started(erocci_core),
    pocci_config(Config).

end_per_suite(_Config) ->
    error_logger:delete_report_handler(cth_log_redirect),
    application:stop(erocci_core),
    error_logger:add_report_handler(cth_log_redirect),
    ok.

all() ->
	[
	 'OCCI_CORE_CREATE_001'
	,'OCCI_CORE_CREATE_006'
	,'OCCI_CORE_DELETE_001'
	,'OCCI_CORE_DISCOVERY_001'
	,'OCCI_CORE_DISCOVERY_002'
	,'OCCI_CORE_MISC_001'
	,'OCCI_CORE_READ_001'
	,'OCCI_CORE_READ_002'
	,'OCCI_CORE_READ_007'
	,'OCCI_CORE_UPDATE_001'
	,'OCCI_INFRA_CREATE_001'
	,'OCCI_INFRA_CREATE_002'
	,'OCCI_INFRA_CREATE_003'
	,'OCCI_INFRA_CREATE_004'
	,'OCCI_INFRA_CREATE_005'
	,'OCCI_INFRA_CREATE_006'
	,'OCCI_INFRA_CREATE_007'
	].


'OCCI_CORE_CREATE_001'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_CORE_CREATE_006'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_CORE_DELETE_001'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_CORE_DISCOVERY_001'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_CORE_DISCOVERY_002'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_CORE_MISC_001'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_CORE_READ_001'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_CORE_READ_002'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_CORE_READ_007'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_CORE_UPDATE_001'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_INFRA_CREATE_001'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_INFRA_CREATE_002'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_INFRA_CREATE_003'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_INFRA_CREATE_004'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_INFRA_CREATE_005'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_INFRA_CREATE_006'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).


'OCCI_INFRA_CREATE_007'(Config) ->
	Cmd = pocci("OCCI/CORE/CREATE/001", Config),
	?assertCmdOutput("OCCI/CORE/READ/001  OK", Cmd).

%%%
%%% Priv
%%%
get_data_path(Path, Config) ->
    DataDir = ?config(data_dir, Config),
    filename:join([DataDir, Path]).

pocci(Name, Config) ->
	Cmd = ?config(pocci, Config) 
		++ " --auth-type ''"
		++ " --url " ++ ?ENDPOINT
		++ " --ignore-ssl"
		++ " --mime-type 'text/plain'"
		++ " --format plain"
		++ " --tests " ++ Name,
	ct:log(info, "cmd = ~s", [Cmd]),
	Cmd.

pocci_config(Config) ->
	PocciConfig = filename:join([?config(data_dir, Config), "pocci.conf"]),
	{ok, [{'POCCI', Path}]} = file:consult(PocciConfig),
	[ {pocci, Path} | Config ].
