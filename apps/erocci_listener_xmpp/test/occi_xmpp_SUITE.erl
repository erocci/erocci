%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @author Lei Zhao <leizhao0515@gmail.com>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_xmpp_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-include("occi.hrl").
-include_lib("erim/include/exmpp_client.hrl").

-define(NAME,"http://localhost:8080").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,5}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:start(occi_core),
    DataDir = proplists:get_value(data_dir, Config),
    Schemas = {schemas, [{path, DataDir ++ "occi-infrastructure.xml"}]},
    Backends = {backends, 
                [{mnesia, occi_backend_mnesia, [Schemas], <<"/">>}]},
    Listeners = {listeners, 
                 [{xmpplocal, occi_xmpp_client, [{jid, "test@local"}]}]
                },
    Acls = {acl, [
                  {allow, '_', '_', '_'}
                 ]},    
    occi:config([{name, ?NAME}, Backends, Listeners, Acls]),    
    Config.

connect(Config) ->
    {ok, Sock} = gen_tcp:connect("localhost", 5562, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]),
    Stream = <<"<stream:stream xmlns='jabber:client' from='test@local' to='test@local' version='1.0'  xmlns:stream='http://etherx.jabber.org/streams'>">>,    
    gen_tcp:send(Sock, Stream),
    gen_tcp:recv(Sock, 0),
    Config1 = [{sock, Sock} | Config],
    Config1.
    
%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {saveConfig,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(occi_core),
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
%%               void() | {saveConfig,Config1}
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
%%               void() | {saveConfig,Config1} | {fail,Reason}
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
%% get_query, 
groups() ->
    [
     {test_all, [], [put_resource_new, put_resource]},
     {create_test_resources, [], [ put_resource_new]},
     {test_xmpp, [],
      [{group,test_resource}]},
     {test_resource, [],
      [ put_resource_new, put_resource, get_resource, post_resource_new, post_resource, 
        get_resource, delete_resource, get_resource_delete, 
        {group, test_link} ]},
     {test_link, [],
      [ {group, create_test_resources}, put_resources, put_link_new, put_link, get_link, post_link_new, post_link, 
        get_link, delete_link, get_link_delete, delete_resource,
        {group, test_kind_col} ]},
     {test_kind_col, [],
      [ put_kind_col,
        {group,test_mixin} ]},
     {test_mixin, [],
      [ {group, create_test_resources}, put_mixin, get_resource,
        {group,test_query} ]},
     {test_query, [],
      [ put_link_new, get_query, 
        {group, test_dir} ]},
     {test_dir, [],
      [ put_resource_dir, get_dir, delete_link,
        {group, test_link2} ]},     
     {test_link2, [],
      [put_resource2, put_link2, put_resource, put_link_new, get_resource,
       {group, test_delete} ]},
     {test_delete, [], [delete_all]}
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
     {group, test_xmpp}
    ].   

%
% @doc Test creation of a new resource on a new ID. 
% expect: "result"
% %end
%
put_resource_new(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "resource1.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    gen_tcp:close(Sock),
    ?assertEqual(Result, <<"result">>).

%
% @doc Test creation of a resource on an existing ID
% expect: "error"
% @end
%
put_resource(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "resource1.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"error">>).

%
% @doc Test creation of a new resource on a new ID. 
% expect: "result"
% %end
%
put_resource2(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "storage_res1.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test creation of a new link on a new ID. 
% expect: "result"
% %end
%
put_link2(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "storage_link.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test creation of three resources for the test of link.
% expect: "errorresultresult"
% end
% 
put_resources(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "resource1.xml",
    FileName1 = proplists:get_value(data_dir, Config) ++ "network1.xml",
    FileName2 = proplists:get_value(data_dir, Config) ++ "resource3.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    {ok, File1} = file:read_file(FileName1),
    gen_tcp:send(Sock, File1),
    case gen_tcp:recv(Sock, 0) of
        {ok, V1} ->  [B2 | _T2] = erim_xml:parse_document(V1),
                     Result1 = erim_xml:get_attribute(B2, <<"type">>, <<"not found">>);
        {error, _Result1} ->
            Result1 = <<"ko">>
    end,
    {ok, File2} = file:read_file(FileName2),
    gen_tcp:send(Sock, File2),
    case gen_tcp:recv(Sock, 0) of
        {ok, V2} ->  [B3 | _T3] = erim_xml:parse_document(V2),
                     Result2 = erim_xml:get_attribute(B3, <<"type">>, <<"not found">>);
        {error, _Result2} ->
            Result2 = <<"ko">>
    end,
    Result3 = <<Result/binary, Result1/binary, Result2/binary>>,
    ?assertEqual(Result3, <<"errorresultresult">>).

% 
% @doc Test creation of a link on a new ID.
% expect: "result"
% end
%
put_link_new(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "link1.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

% 
% @doc Test creation of a link on an existing ID.
% expect: "error"(conflict)
% end
%
put_link(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "link1.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"error">>).

% 
% @doc Test creation of a resource with mixin.
% expect: "result"
% end
%
put_mixin(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "res_mixin1.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test creation of a resource by updating a kind collection.
% expect: "error"(method not allowed)
% end

put_kind_col(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "kind1.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"error">>).

%
% @doc Test updating of an existing resource 
% expect: "result(ok)
% end 
%
post_resource_new(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "update_res.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test updating of a resource on an ID which is not existing.
% expect: "error"(not found)
% end
%
post_resource(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "ko_update.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"error">>).

%
% @doc Test updating of a existing link.
% expect: "result"
% end
%  
post_link_new(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "update_link.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test creation of a link on an ID which is not existing.
% expect: "error"(not found)
% end 
%
post_link(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "ko_update_link.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"error">>).

%
% @doc Test deletion of URL
% expect: "result"(no content)
% end
%
delete_resource(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "delete_res.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test deleting of URL
% expect: "result"
% end
%
delete_link(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "delete_link.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test obtaining the infomation of an ID for testing if PUT and POST work. 
% expect: "result"(ok)
% end
%
get_resource(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "get_res.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test obtaining the infomation of an ID for testing if DELETE works.
% expect: "error"(not found)
% end
%
get_resource_delete(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "get_res.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"error">>).

%
% @doc Test obtaining the information of an ID for testing if PUT and POST work. 
% expect: "result"(ok)
% end
%
get_link(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "get_link.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test obtaining the infomation of an ID for testing if DELETE works.
% expect: "error"(not found)
% end
%
get_link_delete(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "get_link.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"error">>).

%
% @doc Test obtaining all the information. 
% expect: "result"(ok)
% end
%
get_query(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "get_all.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test creation of two resources on differents ID for testing get_dir.
% expect: 201"errorresult"
% end
% 
put_resource_dir(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "resource1.xml",
    FileName1 = proplists:get_value(data_dir, Config) ++ "resource_dir.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    {ok, File1} = file:read_file(FileName1),
    gen_tcp:send(Sock, File1),
    case gen_tcp:recv(Sock, 0) of
        {ok, V1} ->  [B2 | _T2] = erim_xml:parse_document(V1),
                     Result1 = erim_xml:get_attribute(B2, <<"type">>, <<"not found">>);
        {error, _Result1} ->
            Result1 = <<"ko">>
    end,
    Result2 = <<Result/binary, Result1/binary>>,
    ?assertEqual(Result2, <<"errorresult">>).

%
% @doc Test obtaining a list of URLs under a directory. 
% expect: "resul"(ok)
% end
%
get_dir(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "get_dir.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    ?assertEqual(Result, <<"result">>).

%
% @doc Test delete link and resource. 
% expect: "resultresult"(ok)
% end
%

delete_all(Config) ->
    FileName = proplists:get_value(data_dir, Config) ++ "delete_all.xml",
    FileName1 = proplists:get_value(data_dir, Config) ++ "delete_all2.xml",
    {ok, File} = file:read_file(FileName),
    Config1 = connect(Config),
    Sock = proplists:get_value(sock, Config1),
    gen_tcp:send(Sock, File),
    case gen_tcp:recv(Sock, 0) of
        {ok, V} ->  [B1 | _T] = erim_xml:parse_document(V),
                    Result = erim_xml:get_attribute(B1, <<"type">>, <<"not found">>);
        {error, _Result} ->
            Result = <<"ko">>
    end,
    {ok, File1} = file:read_file(FileName1),
    gen_tcp:send(Sock, File1),
    case gen_tcp:recv(Sock, 0) of
        {ok, V1} ->  [B2 | _T2] = erim_xml:parse_document(V1),
                     Result1 = erim_xml:get_attribute(B2, <<"type">>, <<"not found">>);
        {error, _Result1} ->
            Result1 = <<"ko">>
    end,
    Result2 = <<Result/binary, Result1/binary>>,
    ?assertEqual(Result2, <<"resultresult">>).
