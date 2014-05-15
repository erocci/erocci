%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @author Lei Zhao <leizhao0515@gmail.com>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_http_json_SUITE).

-compile(export_all).

-compile([{parse_transform,lager_transform}]).

-define(test_id, 'http://localhost:8080/myresources').

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

-include_lib("kernel/include/file.hrl").

-include("occi.hrl").

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
    DataDir = proplists:get_value(data_dir, Config),
    Schemas = {schemas, [{xml, DataDir ++ "occi-infrastructure.xml"}]},
    Backends = {backends, 
		[{mnesia, occi_backend_mnesia, [Schemas], "/"}]},
    Listeners = {listeners, 
		 [{http, occi_http, [{port, 8080}]}]
		},
    occi:config([{name, "http://localhost:8080"}, Backends, Listeners]),
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
init_per_testcase(TestCase, Config) ->
    
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(TestCase, _Config) ->    
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
    [
     {test_json,
      [],
      [{group,test_resource}]},
     {test_resource,
      [],
      [put_resource_new,put_resource,get_resource_put,post_resource_new,post_resource,get_resource_post,delete_resource,get_resource_delete%%,{group,test_link}
      ]},
     {test_link,
      [],
      [put_link_new,put_link,get_link,post_link_new,post_link,get_link,delete_link,get_link,{group,test_kind}
      ]},
     {test_kind,
      [],
      [put_kind,get_kind,post_kind,get_kind,delete_kind,get_kind,{group,test_mixin}
      ]},
     {test_mixin,
      [],
      [put_mixin,get_mixin,post_mixin,get_mixin,delete_mixin,get_mixin,{group,test_query}
      ]},
     {test_query,
      [],
      [put_query,get_query,post_query,get_query,delete_query,get_query,{group,test_dir}
      ]},
     {test_dir,
      [],
      [put_dir,post_dir,delete_dir]}
    ].

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
     {group,test_json}
    ].   


put_resource_new(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("#####PUT_RESOURCE_NEW ~p",[Headers]).
    
put_resource(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources",
    lager:info("####FILE : ~p",[File]),
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(409,Code),
    lager:info("#####PUT_RESOURCE ~p",[Headers]).

put_link_new(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("####FILE_LINK : ~p",[File]),
    lager:info("#####PUT_LINK_NEW ~p",[Headers]).

put_link(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(409,Code),
    lager:info("#####PUT_LINK ~p",[Headers]).

put_kind(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "kind1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/compute",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("#####PUT_KIND ~p",[Headers]).

put_mixin(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "mixin1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("#####PUT_MIXIN ~p",[Headers]).

put_query(_Config) ->
    
    ok.

put_dir(_Config) ->

    ok.

post_resource_new(_Config) ->
    Id = "http://localhost:8080/myresources",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(post,{Id,[],"application/json","{\n    \"resources\": [\n        {\n            \"kind\": \"http://schemas.ogf.org/occi/infrastructure#compute\",\n            \"attributes\": {\n                \"occi\":{\n\t\t\t\"compute\":{\n\t\t\t\t\"speed\": 2,\n                  \t\t\"memory\": 2,\n                \t\t\"cores\": 2\n            \t\t}\n\t\t}\n            }\n        }\n    ]\n}\n"},[],[]),
    ?assertEqual(200,Code),
    lager:info("#####POST_RESOURCE_NEW ~p",[Headers]).

post_resource(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res2.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources1",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(post,{Id,[],"application/json",File},[],[]),
    ?assertEqual(404,Code),
    lager:info("#####POST_RESOURCE ~p",[Headers]).
    

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
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code),
    lager:info("#####DELETE_RESOURCE  ~p",[Headers]).

delete_link(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code),
    lager:info("#####DELETE_LINK  ~p",[Headers]).

delete_kind(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "kind1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/compute",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code),
    lager:info("#####DELETE_KIND  ~p",[Headers]).

delete_mixin(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "mixin1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code),
    lager:info("#####DELETE_MIXIN  ~p",[Headers]).

delete_query(_Config) ->
    
    ok.

delete_dir(_Config) ->
    ok.

get_resource_put(_Config) ->
    Id = "http://localhost:8080/myresources", 
    {ok, {{_Protocol,Code,_Status}, Headers, Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),  
    ?assertEqual(200, Code),
    ?assert(lists:member({"location", Id}, Headers)).

get_resource_post(_Config) ->
    Id = "http://localhost:8080/myresources", 
    {ok, {{_Protocol,Code,_Status}, Headers, Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),  
    ?assertEqual(200, Code),
    ?assert(lists:member({"location", Id}, Headers)).


get_resource_delete(_Config) ->
    Id = "http://localhost:8080/myresources",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_RESOURCE_DELETE:::~p",[Headers]),
    ?assertEqual(404, Code).

get_link(_Config) ->
    Id = "http://localhost:8080/myresources",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_LINK:::~p",[Headers]),
    ?assertEqual(200, Code),
    ?assert(lists:member({"location", Id}, Headers)).

get_kind(_Config) ->
    Id = "http://localhost:8080/compute",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_KIND:::~p",[Headers]),
    ?assertEqual(200, Code),
    ?assert(lists:member({"location", Id}, Headers)).


get_mixin(_Config) ->
     Id = "http://localhost:8080/myresources",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_LINK:::~p",[Headers]),
    ?assertEqual(200, Code),
    ?assert(lists:member({"location", Id}, Headers)).


get_query(_Config) ->
    {ok,{{_Protocol, Code, _Status}, Headers, _Body}} = httpc:request(get,{"http://localhost:8080/-/",[{"accept","application/json"}]},[],[]),
    ?assertEqual(200,Code),
    ?assert(lists:member({"content-type","application/json"}, Headers)).


