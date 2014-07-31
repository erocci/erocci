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
    application:set_env(lager, handlers, [{lager_console_backend, debug}]),
    application:start(occi),
    DataDir = proplists:get_value(data_dir, Config),
    Schemas = {schemas, [{xml, DataDir ++ "occi-infrastructure.xml"}]},
    Backends = {backends, 
		[{mnesia, occi_backend_mnesia, [Schemas], <<"/">>}]},
    Listeners = {listeners, 
		 [{http, occi_http, [{port, ?PORT}]}]
		},
    Acls = {acl, [
		  {allow, delete, capabilities, owner},
		  {allow, '_', capabilities, '_'},
		  {allow, create, '_', '_'},
		  {allow, read, '_', group},
		  {allow, update, '_', owner},
		  {allow, delete, '_', owner},
		  {deny, '_', '_', '_'}
		 ]},
    occi:config([{name, ?NAME}, Backends, Listeners, Acls]),
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
    [
     {create_test_resources, [], [ put_resources, put_link_new ]},
     {test_json, [],
      [{group,test_resource}]},
     {test_resource, [],
      [ put_resource_new, put_resource, get_resource, post_resource_new, post_resource, 
	get_resource, delete_resource, get_resource_delete, 
	{group, test_link} ]},
     {test_link, [],
      [ {group, create_test_resources}, put_link, get_link, post_link_new, post_link, 
	get_link, delete_link, get_link_delete, 
	{group, test_kind_col} ]},
     {test_kind_col, [],
      [ put_kind_col, get_kind_col, post_kind_col, get_kind_col, delete_kind_col, 
	{group,test_mixin_col} ]},
     {test_mixin_col, [],
      [ {group, create_test_resources}, put_mixin_col, get_mixin_col, post_mixin_col, 
	get_mixin_col, delete_mixin_col, {group,test_query} ]},
     {test_query, [],
      [ get_query, 
	{group, test_dir} ]},
     {test_dir, [],
      [ put_resource_dir, get_dir ]}
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

%
% @doc Test creation of a new resource on a new ID. 
% expect: 201(created)
% %end
%
put_resource_new(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "resource1.json",
    {ok, File} = file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(put, {Id, [], "application/json", File}, [], []),
    ?assertEqual(201, Code).

%
% @doc Test creation of a resource on an existing ID
% expect: 409 (conflict)
% @end
%
put_resource(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "resource1.json",
    {ok, File} = file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(put, {Id, [], "application/json", File}, [], []),
    ?assertEqual(409, Code).

%
% @doc Test creation of three resources for the test of link.
% expect: 201(created)
% end
% 
put_resources(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "resource1.json",
    FileName1 = proplists:get_value(data_dir, _Config) ++ "resource2.json",
    FileName2 = proplists:get_value(data_dir, _Config) ++ "resource3.json",
    {ok, File} = file:read_file(FileName),
    {ok, File1} = file:read_file(FileName1),
    {ok, File2} = file:read_file(FileName2),
    Id = ?NAME ++ "/myresources/id",
    Id1 = ?NAME ++ "/myresources/id2",
    Id2 = ?NAME ++ "/myresources/id3",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(put, {Id, [], "application/json", File}, [], []),
    {ok, {{_Protocol1, Code1, _Status1}, _Headers1, _Body1}} =  
	httpc:request(put, {Id1, [], "application/json", File1}, [], []),
    {ok, {{_Protocol2, Code2, _Status2}, _Headers2, _Body2}} =  
	httpc:request(put, {Id2, [], "application/json", File2}, [], []),
    ?assertEqual(201, Code),
    ?assertEqual(201, Code1),
    ?assertEqual(201, Code2).

% 
% @doc Test creation of a link on a new ID.
% expect: 201(created)
% end
%
put_link_new(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok, File} = file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id4",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(put, {Id, [], "application/json", File}, [], []),
    ?assertEqual(201, Code).

% 
% @doc Test creation of a link on an existing ID.
% expect: 409(conflict)
% end
%
put_link(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok, File} = file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id4",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(put, {Id, [], "application/json", File}, [], []),
    ?assertEqual(409, Code).

%
% @doc Test creation of a resource by updating a kind collection.
% expect: 405(method not allowed)
% end
%
put_kind_col(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "kind1.json",
    {ok, File} = file:read_file(FileName),
    Id = ?NAME ++ "/collections/compute/",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(put, {Id, [], "application/json", File}, [], []),
    ?assertEqual(405, Code).

%
% @doc Test creation of a resource by updating a mixin collection.
% expect: 201(created)
% end
%
put_mixin_col(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "mixin1.json",
    {ok, File} = file:read_file(FileName),
    Id = ?NAME ++ "/collections/os_tpl/",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(put, {Id, [], "application/json", File}, [], []),   
    ?assertEqual(204, Code).

%
% @doc Test updating of an existing resource 
% expect: 200(ok)
% end 
%
post_resource_new(_Config) ->
    Id = ?NAME ++ "/myresources/id",
    Content = "{ \"resources\": [  {  \"kind\": \"http://schemas.ogf.org/occi/infrastructure#compute\", "
	++ " \"attributes\": {  \"occi\":{\"compute\":{\"speed\": 2,\"memory\": 2,\"cores\": 2}} } } ]}",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(post, {Id, [],"application/json", Content}, [], []),
    ?assertEqual(200, Code).

%
% @doc Test creation of a resource on an ID which is not existing.
% expect: 404(not found)
% end
%
post_resource(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "resource1.json",
    {ok, File} = file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id11",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(post, {Id, [], "application/json", File}, [], []),
    ?assertEqual(404, Code).
 
%
% @doc Test updating of a existing link.
% expect: 204(no content)
% end
%  
post_link_new(_Config) ->
    Id = ?NAME ++ "/myresources/id4",
    Content = "{ \"links\": [{  \"kind\": \"http://schemas.ogf.org/occi/infrastructure#networkinterface\", "
	++ "\"attributes\": {\"occi\": {  \"core\": {\"source\": \"" 
	++ ?NAME ++ "/myresources/id\",\"target\": \"" 
	++ ?NAME ++ "/myresources/id3\" },  \"networkinterface\": {\"interface\": \"eth0\",\"mac\": "
	++ "\"00:80:41:ae:fd:7e\" } } } } ]}",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(post, {Id, [], "application/json", Content}, [], []),
    ?assertEqual(200, Code).

%
% @doc Test creation of a link on an ID which is not existing.
% expect: 404(not found)
% end 
%
post_link(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "link1.json",
    {ok, File} = file:read_file(FileName),
    Id = ?NAME ++ "/myresources/id11",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(post, {Id, [], "application/json", File}, [], []),
    ?assertEqual(404, Code).

%
% @doc Test creation of a resource by updating a kind collection.
% expect: 200(ok)
% end
%
post_kind_col(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "kind2.json",
    {ok, File} = file:read_file(FileName),
    Id = ?NAME ++ "/collections/compute/",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(post, {Id, [], "application/json", File}, [], []),
    ?assertEqual(200, Code).
  
%
% @doc Test creation of a resource by updating a mixin collection.
% expect: 200(ok)
% end
%
post_mixin_col(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "mixin2.json",
    {ok, File} = file:read_file(FileName),
    Id = ?NAME ++ "/collections/os_tpl/",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} = 
	httpc:request(post, {Id, [], "application/json", File}, [], []),
    ?assertEqual(204, Code).

%
% @doc Test deletion of URL
% expect: 204(no content)
% end
%
delete_resource(_Config) ->
    Id = ?NAME ++ "/myresources/id",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(delete, {Id, []}, [], []),
    ?assertEqual(204, Code).

%
% @doc Test deleting of URL
% expect: 204(no content)
% end
%
delete_link(_Config) ->
    Id = ?NAME ++ "/myresources/id4",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(delete, {Id, []}, [], []),
    ?assertEqual(204, Code).

%
% @doc Test deleting of URL
% expect: 204(no content)
% end
%
delete_kind_col(_Config) ->
    Id = ?NAME ++ "/collections/compute/",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(delete, {Id, []}, [], []),
    ?assertEqual(204, Code).

%
% @doc Test deleting of URL
% expect: 204(no content)
% end
%
delete_mixin_col(_Config) ->
    Id = ?NAME ++ "/collections/os_tpl/",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} =  
	httpc:request(delete, {Id, []}, [], []),
    ?assertEqual(204, Code).

%
% @doc Test obtaining the infomation of an ID for testing if PUT and POST work. 
% expect: 200(ok)
% end
%
get_resource(_Config) ->
    Id = ?NAME ++ "/myresources/id", 
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = 
	httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),  
    ?assertEqual(200, Code).

%
% @doc Test obtaining the infomation of an ID for testing if DELETE works.
% expect: 404(not found)
% end
%
get_resource_delete(_Config) ->
    Id = ?NAME ++ "/myresources/id",
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = 
	httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    ?assertEqual(404, Code).

%
% @doc Test obtaining the information of an ID for testing if PUT and POST work. 
% expect: 200(ok)
% end
%
get_link(_Config) ->
    Id = ?NAME ++ "/myresources/id4",
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = 
	httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    ?assertEqual(200, Code).

%
% @doc Test obtaining the infomation of an ID for testing if DELETE works.
% expect: 404(not found)
% end
%
get_link_delete(_Config) ->
    Id = ?NAME ++ "/myresources/id4",
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = 
	httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    ?assertEqual(404, Code).

%
% @doc Test obtaining the information of an ID for testing if PUT and POST work. 
% expect: 200(ok)
% end
%
get_kind_col(_Config) ->
    Id = ?NAME ++ "/collections/compute/",
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = 
	httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    ?assertEqual(200, Code).

%
% @doc Test obtaining the information of an ID for testing if PUT and POST work. 
% expect: 200(ok)
% end
%
get_mixin_col(_Config) ->
     Id = ?NAME ++ "/collections/os_tpl/",
    {ok, {{_Protocol,Code,_Status}, _Headers, _Body}} = 
	httpc:request(get, {Id, [{"accept","application/json"}]}, [], []),   
    ?assertEqual(200, Code).

%
% @doc Test obtaining all the information. 
% expect: 200(ok)
% end
%
get_query(_Config) ->
    {ok, {{_Protocol, Code, _Status}, Headers, _Body}} = httpc:
	request(get, {?NAME ++ "/-/", [{"accept","application/json"}]}, [], []),
    ?assertEqual(200, Code),
    ?assert(lists:member({"content-type","application/json"}, Headers)).

%
% @doc Test creation of two resources on differents ID for testing get_dir.
% expect: 201(created)
% end
% 
put_resource_dir(_Config) ->
    FileName = proplists:get_value(data_dir, _Config) ++ "resource1.json",
    FileName1 = proplists:get_value(data_dir, _Config) ++ "resource2.json",
    {ok, File} = file:read_file(FileName),
    {ok, File1} = file:read_file(FileName1),
    Id = ?NAME ++ "/myresources/id1",
    Id1 = ?NAME ++ "/myresources/subdir/id5",
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} = 
	httpc:request(put, {Id, [], "application/json", File}, [], []),
    {ok, {{_Protocol1, Code1, _Status1}, _Headers1, _Body1}} =
	httpc:request(put, {Id1, [], "application/json", File1}, [], []),
    ?assertEqual(201, Code),
    ?assertEqual(201, Code1).

%
% @doc Test obtaining a list of URLs under a directory. 
% expect: 200(ok)
% end
%
get_dir(_Config) ->
    {ok, {{_Protocol, Code, _Status}, _Headers, _Body}} = 
	httpc:request(get,{?NAME ++ "/myresources/", [{"accept","application/json"}]}, [], []),
    ?assertEqual(200, Code).
