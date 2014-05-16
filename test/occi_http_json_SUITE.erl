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
      [put_resource_new,
       %put_resource,    Code http: {expected,409}, {value,200}
       %get_resource,    We can't find it in "application/json", it's in the "text/plain"
       post_resource_new,post_resource,
       %get_resource,    We can't find it in "application/json", it's in the "text/plain"
       delete_resource,get_resource_delete,{group,test_link}
      ]},
     {test_link,
      [],
      [put_resources,put_link_new,
       %put_link,    Code http: {expected,409}, {value,200}
       %get_link,     We can't find it in "application/json", it's in the "text/plain"
       %post_link_new,    Code http: {expected,200}, {value,500}
       post_link,
       %get_link,     We can't find it in "application/json", it's in the "text/plain"
       delete_link,get_link_delete
       %%,{group,test_kind}    Stop test here.
      ]},
     {test_kind,
      [],
      [put_kind,get_kind,post_kind,get_kind,delete_kind,get_kind_delete,{group,test_mixin}
      ]},
     {test_mixin,
      [],
      [put_mixin,get_mixin,post_mixin,get_mixin,delete_mixin,get_mixin_delete,{group,test_query}
      ]},
     {test_query,
      [],
      [get_query,{group,test_dir}
      ]},
     {test_dir,
      [],
      [
      %% put_dir,
       %%post_dir
      ]}
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
    Id = "http://localhost:8080/myresources/id",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("#####PUT_RESOURCE_NEW ~p",[Headers]).
    
put_resource(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources/id",
    lager:info("####FILE : ~p",[File]),
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(409,Code),
    lager:info("#####PUT_RESOURCE ~p",[Headers]).

put_resources(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    FileName1=proplists:get_value(data_dir, _Config) ++ "res2.json",
    FileName2=proplists:get_value(data_dir, _Config) ++ "res3.json",
    {ok,File}=file:read_file(FileName),
    {ok,File1}=file:read_file(FileName1),
    {ok,File2}=file:read_file(FileName2),
    Id = "http://localhost:8080/myresources/id",
    Id1 = "http://localhost:8080/myresources/id2",
    Id2 = "http://localhost:8080/myresources/id3",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    {ok,{{_Protocol1,Code1,_Status1},Headers1,_Body1}} =  httpc:request(put,{Id1,[],"application/json",File1},[],[]),
    {ok,{{_Protocol2,Code2,_Status2},Headers2,_Body2}} =  httpc:request(put,{Id2,[],"application/json",File2},[],[]),
    ?assertEqual(201,Code),
    ?assertEqual(201,Code1),
    ?assertEqual(201,Code2),
    lager:info("#####PUT_RESOURCESSSS ~p",[Headers]),
    lager:info("#####PUT_RESOURCESSSS ~p",[Headers1]),
    lager:info("#####PUT_RESOURCESSSS ~p",[Headers2]).

put_link_new(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources/id4",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("####FILE_LINK : ~p",[File]),
    lager:info("#####PUT_LINK_NEW ~p",[Headers]).

put_link(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources/id4",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(409,Code),
    lager:info("#####PUT_LINK ~p",[Headers]).


put_kind(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res2.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/collections/compute",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("####FILE_KIND : ~p",[File]),
    lager:info("#####PUT_KIND ~p",[Headers]).

put_mixin(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "mixin1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/collections/compute",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("#####PUT_MIXIN ~p",[Headers]).

put_query(_Config) ->
    Id = "http://localhost:8080/myresources/id",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",{}},[],[]),
    ?assertEqual(405,Code).

put_dir(_Config) ->
    Id = "http://localhost:8080/myresources/id",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",{}},[],[]),
    ?assertEqual(405,Code).

post_resource_new(_Config) ->
    Id = "http://localhost:8080/myresources/id",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(post,{Id,[],"application/json","{\n    \"resources\": [\n        {\n            \"kind\": \"http://schemas.ogf.org/occi/infrastructure#compute\",\n            \"attributes\": {\n                \"occi\":{\n\t\t\t\"compute\":{\n\t\t\t\t\"speed\": 2,\n                  \t\t\"memory\": 2,\n                \t\t\"cores\": 2\n            \t\t}\n\t\t}\n            }\n        }\n    ]\n}\n"},[],[]),
    ?assertEqual(200,Code),
    lager:info("#####POST_RESOURCE_NEW ~p",[Headers]).

post_resource(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources/id11",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(post,{Id,[],"application/json",File},[],[]),
    ?assertEqual(404,Code),
    lager:info("#####POST_RESOURCE ~p",[Headers]).
    

post_link_new(_Config) ->
    Id = "http://localhost:8080/myresources/id4",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(post,{Id,[],"application/json","{\n    \"links\": [\n\t{\n\t    \"kind\": \"http://schemas.ogf.org/occi/infrastructure#networkinterface\",\n\t    \"attributes\": {\n\t\t\"occi\": {\n\t\t    \"core\": {\n\t\t\t\"source\": \"http://localhost:8080/myresources/id\",\n\t\t\t\"target\": \"http://localhost:8080/myresources/id3\"\n\t\t    },\n\t\t    \"networkinterface\": {\n\t\t\t\"interface\": \"eth0\",\n\t\t\t\"mac\": \"00:80:41:ae:fd:7e\"\n\t\t    }\n\t\t    \n\t\t}\n\t    }\n\t \n\t}\n    ]\n}\n\n\n\n\n"},[],[]),
    ?assertEqual(200,Code),
    lager:info("#####POST_LINK_NEW ~p",[Headers]).

post_link(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources/id11",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(post,{Id,[],"application/json",File},[],[]),
    ?assertEqual(404,Code),
    lager:info("#####POST_LINK ~p",[Headers]).

post_kind(_Config) ->
    Id = "http://localhost:8080/myresources/id2",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(post,{Id,[],"application/json", "{\n    \"kinds\": [\n\t{\n            \"kind\": \"http://schemas.ogf.org/occi/infrastructure#compute\",\n            \"attributes\": {\n               \t \"occi\": {\n\t\t\t\"compute\": {\n\t\t\t\t\"cores\":{\n\t\t\t\t\t\"type\": 2\n\t\t\t\t},\n\t\t\t\t\"hostname\": {\n\t\t                    \"type\": \"string\",\n\t\t                    \"title\": \"System hostname\"\n\t\t\t\t},\n\t\t\t\t\"speed\": {\n\t\t\t\t\t\"type\": \"float\"\n\t\t\t\t},\n\t\t\t\t\"memory\": {\n\t\t\t\t\t\"type\": \"float\"\n\t\t\t\t}\n\t\t\t}\n                 }   \n            },\n\t    \"action\": {\n\t\t\"term\" : \"start\",\n\t\t\"scheme\": \"http://schemas.ogf.org/occi/infrastructure/compute/action#\"\n\t    }\n       }\n       \n    ]\n}\n \n"},[],[]),
    ?assertEqual(200,Code),
    lager:info("#####POST_KIND ~p",[Headers]).
  

post_mixin(_Config) ->
    ok.

post_query(_Config) ->
    Id = "http://localhost:8080/myresources/id",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",{}},[],[]),
    ?assertEqual(404,Code).

post_dir(_Config) ->
    Id = "http://localhost:8080/myresources/id",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",{}},[],[]),
    ?assertEqual(404,Code).

delete_resource(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources/id",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code),
    lager:info("#####DELETE_RESOURCE  ~p",[Headers]).

delete_link(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources/id4",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code),
    lager:info("#####DELETE_LINK  ~p",[Headers]).

delete_kind(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "kind1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources/id2",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code),
    lager:info("#####DELETE_KIND  ~p",[Headers]).

delete_mixin(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "mixin1.json",
    {ok,File}=file:read_file(FileName),
    Id = "http://localhost:8080/myresources/id3",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code),
    lager:info("#####DELETE_MIXIN  ~p",[Headers]).

delete_query(_Config) ->   
    Id ="http://localhost:8080/-/",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",{}},[],[]),
    ?assertEqual(204,Code).

get_resource(_Config) ->
    Id = "http://localhost:8080/myresources/id", 
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),  
    ?assertEqual(200, Code),
    ?assert(lists:member({"location", Id}, Headers)).

get_resource_delete(_Config) ->
    Id = "http://localhost:8080/myresources/id",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_RESOURCE_DELETE:::~p",[Headers]),
    ?assertEqual(404, Code).

get_link(_Config) ->
    Id = "http://localhost:8080/myresources/id4",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_LINK:::~p",[Headers]),
    ?assertEqual(200, Code),
    ?assert(lists:member({"location", Id}, Headers)).

get_link_delete(_Config) ->
    Id = "http://localhost:8080/myresources/id4",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_LINK_DELETE:::~p",[Headers]),
    ?assertEqual(404, Code).

get_kind(_Config) ->
    Id = "http://localhost:8080/compute",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_KIND:::~p",[Headers]),
    ?assertEqual(200, Code),
    ?assert(lists:member({"location", Id}, Headers)).

get_kind_delete(_Config) ->
    Id = "http://localhost:8080/compute",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_KIND_DELETE:::~p",[Headers]),
    ?assertEqual(404, Code).


get_mixin(_Config) ->
     Id = "http://localhost:8080/myresources",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_MIXIN:::~p",[Headers]),
    ?assertEqual(200, Code),
    ?assert(lists:member({"location", Id}, Headers)).

get_mixin_delete(_Config) ->
     Id = "http://localhost:8080/myresources",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_MIXIN_DELETE:::~p",[Headers]),
    ?assertEqual(404, Code).


get_query(_Config) ->
    {ok,{{_Protocol, Code, _Status}, Headers, _Body}} = httpc:request(get,{"http://localhost:8080/-/",[{"accept","application/json"}]},[],[]),
    ?assertEqual(200,Code),
    ?assert(lists:member({"content-type","application/json"}, Headers)).

get_query_delete(_Config) ->
    {ok,{{_Protocol, Code, _Status},_Headers, _Body}} = httpc:request(get,{"http://localhost:8080/-/",[{"accept","application/json"}]},[],[]),
    ?assertEqual(404,Code).

