%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_http_json_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("occi.hrl").

-define(BASE, <<"http://localhost:8080">>).
-define(SCHEME_INFRA, 'http://schemas.ogf.org/occi/infrastructure#').
-define(SCHEME_NET, 'http://schemas.ogf.org/occi/infrastructure/network#').
-define(SCHEME_NET_IF, 'http://schemas.ogf.org/occi/infrastructure/networkinterface#').

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:start(occi),
    DataDir = proplists:lookup(data_dir, Config),
    Mapping = [
	       {#occi_cid{scheme=?SCHEME_INFRA, term='compute', class=kind}, "/compute/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='storage', class=kind}, "/storage/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='storagelink', class=kind}, "/storagelink/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='network', class=kind}, "/network/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='networkinterface', class=kind}, "/networkinterface/"},
	       {#occi_cid{scheme=?SCHEME_NET, term='ipnetwork', class=mixin}, "/ipnetwork/"},
	       {#occi_cid{scheme=?SCHEME_NET_IF, term='ipnetworkinterface', class=mixin}, "/ipnetworkinterface/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='os_tpl', class=mixin}, "/os_tpl/"},
	       {#occi_cid{scheme=?SCHEME_INFRA, term='resource_tpl', class=mixin}, "/resource_tpl/"}
	      ],
    Extensions = {extensions,
		  [{xml, DataDir ++ "/occi-infrastructure.xml"}], 
		  Mapping},
    Backends = {backends, 
		[{mnesia, occi_backend_mnesia, [], "/"}]},
    Listeners = {listeners, 
		 [{http, occi_http, [{port, 8080}]}]
		},
    occi:config([{name, "http://localhost:8080"},
		 Extensions, 
		 Backends,
		 Listeners]),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(occi),
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     put_resource_new,
     put_resource,
     put_link_new,
     put_link,
     put_kind,
     put_mixin,
     put_query,
     put_dir,
     post_resource_new,
     post_resource,
     post_link_new,
     post_link,
     post_kind,
     post_mixin,
     post_query,
     post_dir,
     delete_resource,
     delete_link,
     delete_kind,
     delete_mixin,
     delete_query,
     delete_dir,
     get_resource,
     get_link,
     get_kind,
     get_mixin,
     get_query
    ].    

put_resource_new(_Config) ->
    ok.

put_resource(_Config) ->
    ok.

put_link_new(_Config) ->
    ok.

put_link(_Config) ->
    ok.

put_kind(_Config) ->
    ok.

put_mixin(_Config) ->
    ok.

put_query(_Config) ->
    ok.

put_dir(_Config) ->
    ok.

post_resource_new(_Config) ->
    ok.

post_resource(_Config) ->
    ok.

post_link_new(_Config) ->
    ok.

post_link(_Config) ->
    ok.

post_kind(_Config) ->
    ok.

post_mixin(_Config) ->
    ok.

post_query(_Config) ->
    ok.

post_dir(_Config) ->
    ok.

delete_resource(_Config) ->
    ok.

delete_link(_Config) ->
    ok.

delete_kind(_Config) ->
    ok.

delete_mixin(_Config) ->
    ok.

delete_query(_Config) ->
    ok.

delete_dir(_Config) ->
    ok.

get_resource(_Config) ->
    ok.

get_link(_Config) ->
    ok.

get_kind(_Config) ->
    ok.

get_mixin(_Config) ->
    ok.

get_query(_Config) -> 
    ok.
