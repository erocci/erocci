%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
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
%%% @doc
%%%
%%% @end
%%% Created :  10 March 2015 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_backend_admin).

-behaviour(occi_backend).

-include("occi.hrl").
-include("occi_log.hrl").

%% occi_backend callbacks
-export([init/1,
	 terminate/1]).
-export([update/2,
	 save/2,
	 delete/2,
	 find/2,
	 load/3,
	 action/3]).

-define(cid_system, #occi_cid{scheme= <<"http://erocci.ow2.org/occi/admin#">>, term= <<"system">>, class=kind}).

-define(system_uri, #uri{path="system"}).
-define(root_uri, #uri{path=[]}).

-define(root_node, occi_node:new(?root_uri,
				 occi_collection:new(?root_uri, [?system_uri]))).

-define(schema, filename:join([priv_dir(), "erocci-admin.xml"])).

-record(state, {start}).

%%%===================================================================
%%% occi_backend callbacks
%%%===================================================================
init(#occi_backend{}) ->
    application:start(occi_backend_admin),
    Start = os:timestamp(),
    ?info("priv_dir=~s~n", [priv_dir()]),
    {ok, [{schemas, [{path, ?schema}]}], #state{start=Start}}.


terminate(#state{}) ->
    application:stop(occi_backend_admin),
    ok.


save(State, #occi_node{}) ->
    {ok, State}.


delete(State, #occi_node{}) ->
    {ok, State}.


update(State, #occi_node{}) ->
    {ok, State}.


find(State, #occi_node{type=capabilities}) ->
    {{ok, [occi_capabilities:new()]}, State};

find(State, #occi_node{id=?root_uri}) ->
    {{ok, [?root_node]}, State};

find(#state{start=Start}=State, #occi_node{id=?system_uri}) ->
    Attrs = [{<<"erocci.admin.sysname">>, application:get_env(occi_backend_admin, name, "")},
	     {<<"erocci.admin.sysdescr">>, application:get_env(occi_backend_admin, descr, "")},
	     {<<"erocci.admin.syscontact">>, application:get_env(occi_backend_admin, contact, "")},
	     {<<"erocci.admin.uptime">>, get_uptime(Start)}],
    {ok, Kind} = occi_category_mgr:get(?cid_system),
    Node = occi_node:new(?system_uri, occi_resource:new(?system_uri, Kind, [], Attrs)),
    {{ok, [Node]}, State};
    
find(State, #occi_node{}) ->
    {{ok, []}, State}.


load(State, #occi_node{}=Req, _Opts) ->
    {{ok, Req}, State}.


action(State, #uri{}, #occi_action{}) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_uptime(Start) ->
    round(timer:now_diff(os:timestamp(), Start) / 1000000).

priv_dir() ->
    case code:priv_dir(occi_backend_admin) of
	{error, bad_name} ->
	    filename:join([filename:dirname(code:which(?MODULE)),
			   "..",
			   "priv"]);
	Path ->
	    Path
    end.
