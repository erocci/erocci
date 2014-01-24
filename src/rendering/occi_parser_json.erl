%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
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
%%% @doc An event based JSON parser
%%%
%%% @end
%%% Created : 11 Dec 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser_json).
-compile({parse_transform, lager_transform}).

-behaviour(gen_fsm).

-include("occi.hrl").
-include("occi_parser.hrl").

%% API
-export([start/0,
	 stop/1,
	 parse/2,
	 parse_full/1]).
-export([parse_resource/1,
	 parse_resource/2,
	 parse_user_mixin/1,
	 parse_collection/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% OCCI parsing states
-export([init/3,
	 request/3,
	 resources_req/3,
	 resources/3,
	 resource/3,
	 resource_kind/3,
	 resource_mixins/3,
	 resource_mixin/3,
	 resource_attributes/3,
	 resource_attribute/3,
	 resource_id/3,
	 resource_links/3,
	 resource_link/3,
	 mixins_req/3,
	 mixins/3,
	 mixin/3,
	 mixin_term/3,
	 mixin_scheme/3,
	 mixin_depends/3,
	 mixin_depend/3,
	 mixin_applies/3,
	 mixin_apply/3,
	 mixin_title/3,
	 mixin_location/3,
	 collection/3,
	 eof/3]).

-define(SERVER, ?MODULE).

-record(state, {request        = #occi_request{}    :: occi_request(),
		collection                          :: occi_collection(),
		resource       = undefined          :: term(),
		mixin          = undefined          :: term(),
		link           = undefined          :: term(),
		attrNS         = []                 :: [string()]}).

%%%===================================================================
%%% API
%%%===================================================================
parse_resource(Data) ->
    case parse_full(Data) of
	{error, Reason} ->
	    {error, {parse_error, Reason}};
	{ok, #occi_request{resources=[#occi_resource{}=Res]}} ->
	    {ok, Res};
	_ ->
	    {error, {parse_error, not_a_resource}}
    end.

parse_resource(Data, #occi_kind{id=Cid}) ->
    case parse_resource(Data) of
	{error, Reason} ->
	    {error, {parse_error, Reason}};
	{ok, #occi_resource{cid=Cid}=Res} ->
	    {ok, Res};
	{ok, #occi_resource{}} ->
	    {error, {parse_error, bad_category}}
    end.

parse_user_mixin(Data) ->
    case parse_full(Data) of
	{error, Reason} ->
	    {error, {parse_error, Reason}};
	{ok, #occi_request{mixins=[#occi_mixin{}=Mixin]}} ->
	    case {occi_mixin:get_attr_list(Mixin),
		  occi_mixin:get_actions(Mixin)} of
		{[], []} ->
		    {ok, Mixin};
		Err ->
		    % User mixins can not set attributes or actions
		    lager:error("Invalid request: ~p~n", [Err]),
		    {error, {parse_error, Err}}
	    end;
	Err ->
	    lager:error("Invalid request: ~p~n", [Err]),
	    {error, {parse_error, Err}}
    end.

parse_collection(Data) ->
    case parse_full(Data) of
	{error, Reason} ->
	    {error, {parse_error, Reason}};
	{ok, #occi_request{collection=Coll}} ->
	    {ok, Coll}
    end.

parse_full(<<>>) ->
    {error, invalid_request};
parse_full(Data) ->
    P = start(),
    Res = parse(P, Data),
    stop(P),
    Res.

-spec parse(parser(), binary()) -> parser_result().
parse(#parser{src=#parser{mod=Mod}=Src}, Data) ->
    Mod:parse(Src, Data).

start() ->
    Parser = #parser{mod=?MODULE, stack=[eof], state=#state{}},
    case gen_fsm:start(?MODULE, Parser, []) of
	{ok, Pid} -> 
	    Src = occi_parser_json0:start(Parser#parser{id=Pid}),
	    Parser#parser{id=Pid, src=Src};
	Err -> 
	    lager:error("Error starting occi/json parser: ~p~n", [Err]),
	    throw(Err)
    end.

stop(#parser{id=Ref, src=#parser{mod=Mod}=Src}) ->
    Mod:stop(Src),
    gen_fsm:send_all_state_event(Ref, stop).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init(#parser{}=Parser) ->
    {ok, init, Parser}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(stop, _, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(Event, _From, _StateName, State) ->
    occi_parser:parse_error(Event, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% OCCI parsing states
init(#token{name=objBegin}, _From, Ctx) ->
    {reply, ok, request, Ctx};
init(#token{name=arrBegin}, _From, #parser{state=State}=Ctx) ->
    {reply, ok, collection, 
     ?set_state(Ctx, State#state{collection=occi_collection:new()})};
init(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

request(#token{name=key, data="resources"}, _From, Ctx) ->
    {reply, ok, resources_req, Ctx};
request(#token{name=key, data="collection"}, _From, Ctx) ->
    {reply, ok, collection_req, Ctx};
request(#token{name=key, data="links"}, _From, Ctx) ->
    % TODO
    {reply, {error, invalid_request}, eof, Ctx};
request(#token{name=key, data="action"}, _From, Ctx) ->
    % TODO
    {reply, {error, invalid_request}, eof, Ctx};
request(#token{name=key, data="kinds"}, _From, Ctx) ->
    % TODO
    {reply, {error, invalid_request}, eof, Ctx};
request(#token{name=key, data="mixins"}, _From, Ctx) ->
    {reply, ok, mixins_req, Ctx};
request(#token{name=key, data="actions"}, _From, Ctx) ->
    % TODO
    {reply, {error, invalid_request}, eof, Ctx};
request(#token{name=objEnd}, _From, #parser{state=#state{request=Req}}=Ctx) ->
    {reply, {eof, Req}, eof, Ctx};
request(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resources_req(#token{name=arrBegin}, _From, Ctx) ->
    {reply, ok, resources, Ctx};
resources_req(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resources(#token{name=objBegin}, _From, #parser{state=State}=Ctx) ->
    {reply, ok, resource, Ctx#parser{state=State#state{resource=occi_resource:new()}}};
resources(#token{name=arrEnd}, _From, Ctx) ->
    {reply, ok, request, Ctx};
resources(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).
    
resource(#token{name=key, data="kind"}, _From, Ctx) ->
    {reply, ok, resource_kind, Ctx};
resource(#token{name=key, data="mixins"}, _From, Ctx) ->
    {reply, ok, resource_mixins, Ctx};
resource(#token{name=key, data="attributes"}, _From, Ctx) ->
    {reply, ok, resource_attributes, Ctx};
resource(#token{name=key, data="id"}, _From, Ctx) ->
    {reply, ok, resource_id, Ctx};
resource(#token{name=key, data="links"}, _From, Ctx) ->
    {reply, ok, resource_links, Ctx};
resource(#token{name=objEnd}, _From, #parser{state=#state{resource=Res, request=Req}=State}=Ctx) ->
    {reply, ok, resources, 
     ?set_state(Ctx, State#state{resource=undefined, request=occi_request:add_resource(Req, Res)})};
resource(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_kind(#token{name=value, data=Val}, _From, #parser{state=#state{resource=Res}=State}=Ctx) ->
    case parse_cid(Val) of
	{Scheme, Term} ->
	    case occi_category_mgr:find(#occi_cid{class=kind, scheme=Scheme, term=Term}) of
		[] ->
		    {reply, {error, invalid_cid}, eof, Ctx};
		[Kind] ->
		    Res2 = occi_resource:set_cid(Res, Kind),
		    {reply, ok, resource, 
		     ?set_state(Ctx, State#state{resource=Res2})}
	    end;
	parse_error ->
	    {reply, {error, invalid_cid}, eof, Ctx}
    end;
resource_kind(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_mixins(#token{name=arrBegin}, _From, Ctx) ->
    {reply, ok, resource_mixin, Ctx};
resource_mixins(#token{name=arrEnd}, _From, Ctx) ->
    {reply, ok, resource, Ctx};
resource_mixins(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_mixin(#token{name=value, data=Val}, _From, #parser{state=#state{resource=Res}=State}=Ctx) ->
    case parse_cid(Val) of
	{Scheme, Term} ->
	    case occi_category_mgr:find(#occi_cid{class=mixin, scheme=Scheme, term=Term}) of
		[] ->
		    {reply, {error, invalid_cid}, eof, Ctx};
		[#occi_mixin{}=Mixin] ->
		    Res2 = occi_resource:add_mixin(Res, Mixin),
		    {reply, ok, resource_mixins, 
		     ?set_state(Ctx, State#state{resource=Res2})}
	    end;
	parse_error ->
	    {reply, {error, invalid_cid}, eof, Ctx}
    end;
resource_mixin(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_attributes(#token{name=objBegin}, _From, #parser{state=#state{}=State}=Ctx) ->
    {reply, ok, resource_attribute,
    ?set_state(Ctx, State#state{attrNS=[]})};
resource_attributes(#token{name=objEnd}, _From, Ctx) ->
    {reply, ok, resource, Ctx};
resource_attributes(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_attribute(#token{name=key, data=Val}, _From, #parser{state=#state{attrNS=NS}=State}=Ctx) ->
    {reply, ok, resource_attribute, 
     ?set_state(Ctx, State#state{attrNS=[Val|NS]})};
resource_attribute(#token{name=objBegin}, _From, Ctx) ->
    {reply, ok, resource_attribute, Ctx};
resource_attribute(#token{name=value, data=Val}, _From, 
		   #parser{state=#state{attrNS=[H|T], resource=Res}=State}=Ctx) ->
    Name = build_attr_name([H|T]),
    {reply, ok, resource_attribute, 
     ?set_state(Ctx, State#state{attrNS=T, resource=occi_resource:set_attr_value(Res, Name, Val)})};
resource_attribute(#token{name=objEnd}, _From, #parser{state=#state{attrNS=[_H|T]}=State}=Ctx) ->
    {reply, ok, resource_attribute, 
     ?set_state(Ctx, State#state{attrNS=T})};
resource_attribute(#token{name=objEnd}, _From, #parser{state=#state{attrNS=[]}=State}=Ctx) ->
    {reply, ok, resource,
     ?set_state(Ctx, State#state{attrNS=[]})};
resource_attribute(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_id(#token{name=value, data=Val}, _From, #parser{state=#state{resource=Res}=State}=Ctx) ->
    {reply, ok, resource, 
     ?set_state(Ctx, State#state{resource=occi_resource:set_id(Res, Val)})};
resource_id(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_links(#token{name=arrBegin}, _From, Ctx) ->
    {reply, ok, resource_link, Ctx};
resource_links(#token{name=arrEnd}, _From, Ctx) ->
    {reply, ok, resource, Ctx};
resource_links(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_link(#token{name=value, data=Val}, _From, #parser{state=#state{resource=Res}=State}=Ctx) ->
    {reply, ok, resource_links, 
     ?set_state(Ctx, State#state{resource=occi_resource:add_link(Res, Val)})};
resource_link(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).
    
mixins_req(#token{name=arrBegin}, _From, Ctx) ->
    {reply, ok, mixins, Ctx};
mixins_req(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

mixins(#token{name=objBegin}, _From, #parser{state=State}=Ctx) ->
    {reply, ok, mixin, 
     ?set_state(Ctx, State#state{mixin=occi_mixin:new()})};
mixins(#token{name=arrEnd}, _From, Ctx) ->
    {reply, ok, request, Ctx};
mixins(Token, _From, Ctx) ->
    occi_parse:parse_error(Token, Ctx).

mixin(#token{name=key, data="term"}, _From, Ctx) ->
    {reply, ok, mixin_term, Ctx};
mixin(#token{name=key, data="scheme"}, _From, Ctx) ->
    {reply, ok, mixin_scheme, Ctx};
mixin(#token{name=key, data="depends"}, _From, Ctx) ->
    {reply, ok, mixin_depends, Ctx};
mixin(#token{name=key, data="applies"}, _From, Ctx) ->
    {reply, ok, mixin_applies, Ctx};
mixin(#token{name=key, data="title"}, _From, Ctx) ->
    {reply, ok, mixin_title, Ctx};
mixin(#token{name=key, data="location"}, _From, Ctx) ->
    {reply, ok, mixin_location, Ctx};
mixin(#token{name=objEnd}, _From, #parser{state=#state{mixin=Mixin, request=Req}=State}=Ctx) ->
    {reply, ok, mixins, 
     ?set_state(Ctx, State#state{mixin=undefined, request=occi_request:add_mixin(Req, Mixin)})};
mixin(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

mixin_term(#token{name=value, data=Val}, _From, #parser{state=#state{mixin=Mixin}=State}=Ctx) ->
    {reply, ok, mixin,
     ?set_state(Ctx, State#state{mixin=occi_mixin:set_term(Mixin, Val)})};
mixin_term(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

mixin_scheme(#token{name=value, data=Val}, _From, #parser{state=#state{mixin=Mixin}=State}=Ctx) ->
    {reply, ok, mixin,
     ?set_state(Ctx, State#state{mixin=occi_mixin:set_scheme(Mixin, Val)})};
mixin_scheme(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

mixin_depends(#token{name=arrBegin}, _From, Ctx) ->
    {reply, ok, mixin_depend, Ctx};
mixin_depends(#token{name=arrEnd}, _From, Ctx) ->
    {reply, ok, mixin, Ctx};
mixin_depends(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

mixin_depend(#token{name=value, data=Val}, _From, #parser{state=#state{mixin=Mixin}=State}=Ctx) ->
    case parse_cid(Val) of
	{Scheme, Term} ->
	    case check_cid(Scheme, Term) of
		#occi_cid{}=Cid ->
		    Mixin2 = occi_mixin:add_depends(Mixin, Cid),
		    {reply, ok, mixin_depends, 
		     ?set_state(Ctx, State#state{mixin=Mixin2})};
		error ->
		    {reply, {error, invalid_cid}, eof, Ctx}
	    end;
	parse_error ->
	    {reply, {error, invalid_cid}, eof, Ctx}
    end;
mixin_depend(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

mixin_applies(#token{name=arrBegin}, _From, Ctx) ->
    {reply, ok, mixin_apply, Ctx};
mixin_applies(#token{name=arrEnd}, _From, Ctx) ->
    {reply, ok, mixin, Ctx};
mixin_applies(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

mixin_apply(#token{name=value, data=Val}, _From, #parser{state=#state{mixin=Mixin}=State}=Ctx) ->
    case parse_cid(Val) of
	{Scheme, Term} ->
	    case check_cid(Scheme, Term) of
		#occi_cid{}=Cid ->
		    Mixin2 = occi_mixin:add_applies(Mixin, Cid),
		    {reply, ok, mixin_applies, 
		     ?set_state(Ctx, State#state{mixin=Mixin2})};
		error ->
		    {reply, {error, invalid_cid}, eof, Ctx}
	    end;
	parse_error ->
	    {reply, {error, invalid_cid}, eof, Ctx}
    end;
mixin_apply(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

mixin_title(#token{name=value, data=Val}, _From, #parser{state=#state{mixin=Mixin}=State}=Ctx) ->
    {reply, ok, mixin, 
     ?set_state(Ctx, State#state{mixin=occi_mixin:set_title(Mixin, Val)})};
mixin_title(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

mixin_location(#token{name=value, data=Val}, _From, #parser{state=#state{mixin=Mixin}=State}=Ctx) ->
    Url = occi_config:get_url(Val),
    {reply, ok, mixin, 
     ?set_state(Ctx, State#state{mixin=occi_mixin:set_location(Mixin, Url)})};
mixin_location(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

collection(#token{name=value, data=Val}, _From, #parser{state=#state{collection=Coll}=State}=Ctx) ->
    {reply, ok, collection, 
     ?set_state(Ctx, State#state{collection=occi_collection:add_entity(Coll, occi_config:get_url(Val))})};
collection(#token{name=arrEnd}, _From, #parser{state=#state{request=Req, collection=Coll}=State}=Ctx) ->
    Req2 = occi_request:set_collection(Req, Coll),
    Ctx2 = ?set_state(Ctx, State#state{request=Req}),
    {reply, {eof, Req2}, eof, Ctx2};
collection(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

eof(_E, _F, Ctx) ->
    {stop, eof, Ctx}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_cid(Str) ->
    case string:tokens(Str, "#") of
	[Scheme, Term] ->
	    {list_to_atom(Scheme++"#"), list_to_atom(Term)};
	_ ->
	    parse_error
    end.

build_attr_name(NS) ->
    list_to_atom(string:join(lists:reverse(NS), ".")).

check_cid(Scheme, Term) ->
    case occi_category_mgr:find(#occi_cid{scheme=Scheme, term=Term, _='_'}) of
	[#occi_kind{id=Cid}] ->
	    Cid;
	[#occi_mixin{id=Cid}] ->
	    Cid;
	[] ->
	    parse_error
    end.
