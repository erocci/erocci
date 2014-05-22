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

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

-include_lib("kernel/include/file.hrl").

-include("occi.hrl").

-define(PORT,8080).

-define(NAME,"http://localhost:8080").

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
		 [{http, occi_http, [{port, ?PORT}]}]
		},
    occi:config([{name, ?NAME}, Backends, Listeners]),
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
       %put_resource,      % Code http: {expected,409}, {value,200}
       get_resource,  
       post_resource_new,post_resource,
       get_resource,   
       delete_resource,get_resource_delete,{group,test_link}
      ]},
     {test_link,
      [],
      [put_resources,put_link_new,
       put_link,         %Code http: {expected,409}, {value,200}
       get_link,    
       post_link_new,    %Code http: {expected,200}, {value,500}
       post_link,
       get_link,    
       delete_link,get_link_delete
       ,{group,test_kind_col} 
      ]},
     {test_kind_col,
      [],
      [put_kind_col,get_kind_col,post_kind_col,get_kind_col,delete_kind_col,get_kind_col_delete
      ,{group,test_mixin_col}
      ]},
     {test_mixin_col,
      [],
      [
       put_mixin_col,       %Code http: {expected,200}, {value,500}
       get_mixin_col,
       post_mixin_col,      %Code http: {expected,200}, {value,500}
       get_mixin_col,
       delete_mixin_col,    %Code http: {expected,200}, {value,500}
       get_mixin_col_delete, %Code http: {expected,404}, {value,200}
       {group,test_query}
      ]},
     {test_query,
      [],
      [get_query,{group,test_dir}
      ]},
     {test_dir,
      [],
      [
      put_resource_dir,get_dir  %Code http: {expected,200}, {value,500}
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
    Id = ?NAME ++ "/myresources/id",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("#####PUT_RESOURCE_NEW ~p",[Headers]).
    
put_resource(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id",
    lager:info("####FILE : ~p",[File]),
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(409,Code).

put_resources(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    FileName1=proplists:get_value(data_dir, _Config) ++ "res2.json",
    FileName2=proplists:get_value(data_dir, _Config) ++ "res3.json",
    {ok,File}=file:read_file(FileName),
    {ok,File1}=file:read_file(FileName1),
    {ok,File2}=file:read_file(FileName2),
    Id = ?NAME ++ "/myresources/id",
    Id1 = ?NAME ++ "/myresources/id2",
    Id2 = ?NAME ++ "/myresources/id3",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    {ok,{{_Protocol1,Code1,_Status1},_Headers1,_Body1}} =  httpc:request(put,{Id1,[],"application/json",File1},[],[]),
    {ok,{{_Protocol2,Code2,_Status2},_Headers2,_Body2}} =  httpc:request(put,{Id2,[],"application/json",File2},[],[]),
    ?assertEqual(201,Code),
    ?assertEqual(201,Code1),
    ?assertEqual(201,Code2).

put_link_new(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id4",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("####FILE_LINK : ~p",[File]),
    lager:info("#####PUT_LINK_NEW ~p",[Headers]).

put_link(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id =?NAME ++ "/myresources/id4",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(409,Code).

put_kind_col(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res5.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/collections/compute",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    ?assertEqual(201,Code),
    lager:info("####FILE_KIND : ~p",[File]),
    lager:info("#####PUT_KIND ~p",[Headers]).

put_mixin_col(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res6.json",
    {ok,File}=file:read_file(FileName),
    Id =?NAME ++ "/collections/os_tpl/",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),   
    lager:info("#####PUT_MIXIN ~p",[Headers]),
     ?assertEqual(201,Code).

post_resource_new(_Config) ->
    Id = ?NAME ++ "/myresources/id",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(post,{Id,[],"application/json","{ \"resources\": [  {  \"kind\": \"http://schemas.ogf.org/occi/infrastructure#compute\",  \"attributes\": {  \"occi\":{\"compute\":{\"speed\": 2,\"memory\": 2,\"cores\": 2}} } } ]}"},[],[]),
    ?assertEqual(200,Code).

post_resource(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id11",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(post,{Id,[],"application/json",File},[],[]),
    ?assertEqual(404,Code).
    
post_link_new(_Config) ->
    Id = "http://localhost:8080/myresources/id4",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(post,{Id,[],"application/json","{ \"links\": [{  \"kind\": \"http://schemas.ogf.org/occi/infrastructure#networkinterface\", \"attributes\": {\"occi\": {  \"core\": {\"source\": \"http://localhost:8080/myresources/id\",\"target\": \"http://localhost:8080/myresources/id3\" },  \"networkinterface\": {\"interface\": \"eth0\",\"mac\": \"00:80:41:ae:fd:7e\" } } } } ]}"},[],[]),
    ?assertEqual(200,Code).

post_link(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id11",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(post,{Id,[],"application/json",File},[],[]),
    ?assertEqual(404,Code).

post_kind_col(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res4.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/collections/compute",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(post,{Id,[],"application/json", File},[],[]),
    lager:info("###POST-KIND ~p",[Headers]),
    %%?assert(lists:member({"location",Id}, Headers)),
    ?assertEqual(200,Code).
  

post_mixin_col(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res7.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/collections/os_tpl/",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(post,{Id,[],"application/json",File},[],[]),
     lager:info("#####POST_MIXIN ~p",[Headers]),
    ?assertEqual(200,Code).
   

delete_resource(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code).

delete_link(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id4",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code).

delete_kind_col(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res4.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/collections/compute",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code).

delete_mixin_col(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res6.json",
    {ok,File}=file:read_file(FileName),
    Id = ?NAME ++ "/collections/os_tpl/",
    {ok,{{_Protocol,Code,_Status},Headers,_Body}} =  httpc:request(delete,{Id,[],"application/json",File},[],[]),
    ?assertEqual(204,Code),
    lager:info("#####DELETE_MIXIN  ~p",[Headers]).



get_resource(_Config) ->
    Id = ?NAME ++ "/myresources/id", 
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),  
    ?assertEqual(200, Code).

get_resource_delete(_Config) ->
    Id = ?NAME ++ "/myresources/id",
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    ?assertEqual(404, Code).

get_link(_Config) ->
    Id = ?NAME ++ "/myresources/id4",
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    ?assertEqual(200, Code).

get_link_delete(_Config) ->
    Id = ?NAME ++ "/myresources/id4",
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    ?assertEqual(404, Code).

get_kind_col(_Config) ->
    Id = ?NAME ++ "/collections/compute",
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    ?assertEqual(200, Code).

get_kind_col_delete(_Config) ->
    Id = ?NAME ++ "/collections/compute",
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    ?assertEqual(404, Code).


get_mixin_col(_Config) ->
     Id = ?NAME ++ "/collections/os_tpl/",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_MIXIN:::~p",[Headers]),
    ?assertEqual(200, Code).

get_mixin_col_delete(_Config) ->
     Id = ?NAME ++ "/collections/os_tpl/",
    {ok, {{_Protocol,Code,_Status}, Headers, _Body}} = httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    lager:info("#### GET_MIXIN_DELETE:::~p",[Headers]),
    ?assertEqual(404, Code).


get_query(_Config) ->
    {ok,{{_Protocol, Code, _Status}, Headers, _Body}} = httpc:request(get,{?NAME ++ "/-/",[{"accept","application/json"}]},[],[]),
    ?assertEqual(200,Code),
    ?assert(lists:member({"content-type","application/json"}, Headers)).


put_resource_dir(_Config) ->
    FileName=proplists:get_value(data_dir, _Config) ++ "res1.json",
    FileName1=proplists:get_value(data_dir, _Config) ++ "res4.json",
    {ok,File}=file:read_file(FileName),
    {ok,File1}=file:read_file(FileName1),
    Id = ?NAME ++ "/myresources/id1",
    Id1 = ?NAME ++ "/myresources/subdir/id5",
    {ok,{{_Protocol,Code,_Status},_Headers,_Body}} =  httpc:request(put,{Id,[],"application/json",File},[],[]),
    {ok,{{_Protocol1,Code1,_Status1},_Headers1,_Body1}} =  httpc:request(put,{Id1,[],"application/json",File1},[],[]),
    ?assertEqual(201,Code),
    ?assertEqual(201,Code1).



get_dir(_Config) ->
    {ok,{{_Protocol, Code, _Status}, Headers, Body}} = httpc:request(get,{?NAME ++ "/myresources/",[{"accept","application/json"}]},[],[]),
    lager:info("### GET DIR Headers~p",[Headers]),
    lager:info("### GET DIR Body ~p",[Body]),
    ?assertEqual(200,Code).


