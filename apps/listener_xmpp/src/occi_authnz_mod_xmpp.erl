%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
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
%%% Created : 27 May 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_authnz_mod_xmpp).

-behaviour(occi_authnz).

-export([init/1,
         auth/2, share_group/3, create_group/2, create_ingroup/4,
         create_user/3, delete_user/2, delete_ingroup/2, delete_group/2, get_groups/1, start/2,
         update_user/3, update_group/3,
         get_group/2, get_user/2, get_users/1,
         get_ingroup/2, get_ingroups/1, get_group_user/2]).

-record(state, {table, lists_group, session}).
-type state() :: #state{}.

-spec init([]) -> state().
init([]) ->
    Table = ets:new(table_user, []),
    #state{table = Table, lists_group=[]}.

-spec start(Opts :: term(), state()) -> {true | false, state()}.
start(Opts, State) ->
    {true, State#state{session=Opts}}.

-spec auth({User1 :: binary(), User2 :: binary()}, state()) -> {true | false, state()}.
auth({_User1, User2}, #state{table = Table}=State) ->
    case ets:member(Table, User2) of
        true -> {true, State};
        false -> {false, State}
    end.

-spec share_group(User1 :: binary(), User2 :: binary(), state()) -> {true | false, state()}.
share_group(User, User, State) ->
    {true, State};
share_group(_User1, User2, State) ->
    case element(2, User2) of		         
        <<"undefined">> -> {false, State};
        _ -> {true, State}
    end.

-spec create_group(Group :: term(), state()) -> {ok | {error, term()}, state()}.
create_group(Group, #state{lists_group = Lgroup}=State) ->
    Groups = lists:nth(1, Group),
    L = case lists:member(Groups, Lgroup) of
	    true -> Lgroup;
	    false -> Lgroup ++ Group
	end,
    {ok, State#state{lists_group=L}}.

-spec delete_group(Group :: term(), state()) -> {ok | {error, term()}, state()}.
delete_group(Group, #state{table=Table, lists_group = Lgroup}=State) ->
    case lists:member(Group, Lgroup) of
        true ->
            LIngroups = ets:match(Table, {'$1', '_', '_', Group}),
            lists:foreach(fun(LIngroup) ->
                                  Ingroup = lists:last(LIngroup),
                                  delete_ingroup(Ingroup, State)
                          end, LIngroups),
            Lgroup1 = lists:delete(Group, Lgroup),
            {ok, State#state{lists_group=Lgroup1}};
        false ->{{error, "Group not found"}, State}
    end.

-spec create_ingroup(Idingroup :: term(), User :: term(), Group :: term(), state()) -> {ok | {error, term()}, state()}.
create_ingroup(Idingroup, User, Group, #state{table = Table, lists_group = Lgroup, session=Session}=State) ->
    case ets:member(Table, User) of            
        true -> case lists:member(Group, Lgroup) of
                    true -> 
                        case ets:match(Table, {'_', User, '_', '$1'}) of
                            [] -> Nick = get_nick(Table, User),
                                  send_iq_set_item(User, Nick, [Group], Session),
                                  ets:insert(Table, {Idingroup, User, Nick, Group}),
                                  {ok, State};
                            _ ->LGroup = ets:match(Table, {'_', User, '_', '$1'}),                                   
                                Groups1 = LGroup++[Group],
                                Nick = get_nick(Table, User),
                                send_iq_set_item(User, Nick, Groups1, Session),
                                ets:insert(Table, {Idingroup, User, Nick, Group}),
                                {ok, State}
                        end;
                    false -> {{error, "Group not found"}, State}
                end;
        false -> {{error, "User not found"}, State}
    end.

-spec delete_ingroup(InGroup :: term(), state()) -> {ok | {error, term()}, state()}.
delete_ingroup(InGroup, #state{table = Table, session=Session}=State) ->
    case ets:member(Table, InGroup) of
        true -> Lingroup = ets:lookup(Table, InGroup),
                Tingroup = lists:last(Lingroup),
                Jid = element(2, Tingroup),
                Nick = get_nick(Table, Jid),
                ets:delete(Table, InGroup),
                case ets:match(Table, {'_', Jid, '_', '$1'}) of
                    [] -> send_iq_set_item(Jid, Nick, [], Session);
                    _ ->
                        LGroup = ets:match(Table, {'_', Jid, '_', '$1'}),
                        Group = lists:last(LGroup),
                        send_iq_set_item(Jid, Nick, [Group], Session)
                end,
                {ok, State};
        false -> {{error, "Ingroup not found"}, State}
    end.

-spec update_group(Group :: term(), NGroup :: term(), state()) -> {ok | {error, term()}, state()}.
update_group(Group, NGroup, #state{table = Table, lists_group = Lgroup}=State) ->
    case lists:member(Group, Lgroup) of
        true -> LIngroups = ets:match(Table, {'$1', '$2', '_', Group}),
                lists:foreach(fun(LIngroup) ->
                                      User = lists:nth(2, LIngroup),  
                                      Ingroup = lists:nth(1, LIngroup),
                                      create_ingroup(Ingroup, User, NGroup, State)
                              end, LIngroups),
                lists:delete(Group, Lgroup),
                Lgroup1 = Lgroup++NGroup,
                {ok, State#state{lists_group=Lgroup1}};
        false -> {{error, "Group not found"}, State}
    end.

-spec update_user(Jid :: term(), User :: term(), state()) -> {ok | {error, term()}, state()}.
update_user(Jid, User, #state{table = Table}=State) ->
    case ets:member(Table, User) of
        true -> create:user(Jid, User, State);
        fasle -> {{error, "Group not found"}, State}
    end.

-spec create_user(Jid :: term(), User :: term(), state()) -> {ok | {error, term()}, state()}.
create_user(Jid, User, #state{table = Table, session=Session}=State) ->
    ets:insert(Table, {Jid, User, '_'}),
    send_iq_set_item(Jid, User, [], Session),
    {ok, State}.

-spec delete_user(User :: term(), state()) -> {ok | {error, term()}, state()}.
delete_user(User, #state{table = Table, session=Session}=State) ->
    case ets:member(Table, User) of
        true -> Element = exmpp_xml:element(item),
                Item = exmpp_xml:set_attributes(Element,
                                                [{<<"jid">>, User},
                                                 {<<"subscription">>, "remove"}]),
                Query = exmpp_xml:element("jabber:iq:roster", query),
                Query2 = exmpp_xml:append_child(Query, Item),
                Element2 = exmpp_xml:element("jabber:client", iq),
                Iq = exmpp_xml:set_attributes(Element2,
                                              [{<<"type">>, "set"}]),
                Iq_send = exmpp_xml:append_child(Iq, Query2),
                exmpp_session:send_packet(Session, Iq_send),
                ets:delete(Table, User),
                {ok, State};
        false -> {{error, "User not found"}, State}
    end.

-spec get_groups(state()) -> {term(), state()}.
get_groups(#state{lists_group=Lgroup}=State) ->
    {Lgroup, State}.

-spec get_users(state()) -> {term(), state()}.
get_users(#state{table = Table}=State) ->
    Users = ets:match(Table, {'$1', '_', '_'}),
    {Users, State}.

-spec get_user(User :: term(), state()) -> {term() | {error, term()}, state()}.
get_user(User, #state{table = Table}=State) ->
    case ets:member(Table, User) of
        true -> Nick = get_nick(Table, User),
                Idingroup = ets:match(Table, {'$1', User, '_', '_'}),
                {{Nick, Idingroup}, State};
        false ->
            {{error, "User not found"}, State}
    end.

-spec get_group(Group :: term(), state()) -> {term() | {error, term()}, state()}.
get_group(Group, #state{table = Table, lists_group = Lgroup}=State) ->
    case lists:member(Group, Lgroup) of
        true -> Idingroup = ets:match(Table, {'$1', '_', '_', Group}),
                {{Group, Idingroup}, State};
        false ->
            {{error, "Group not found"}, State}
    end.

-spec get_ingroups(state()) -> {term() | {error, term()}, state()}.
get_ingroups(#state{table = Table}=State) ->
    Idingroups = ets:match(Table, {'$1', '_', '_', '_'}),
    {Idingroups, State}.

-spec get_ingroup(InGroup :: term(), state()) -> {term() | {error, term()}, state()}.
get_ingroup(InGroup, #state{table = Table}=State) ->
    case ets:member(Table, InGroup) of
        true ->
            IGroup = ets:match(Table, {InGroup, '$1', '_', '$2'}),
            List = lists:last(IGroup),
            User = lists:nth(1, List),
            Group = lists:nth(2, List),
            {{User, Group}, State};
        false -> {{error, "InGroup not found"}, State}
    end.

-spec get_group_user(User :: term(), state()) -> {term(), state()}.
get_group_user(User, #state{table=Table}=State) ->
    Group = ets:match(Table, {'_', User, '_', '$1'}),
    case ets:match(Table, {'_', User, '_', '$1'}) of
        [] -> {"undefined", State};
        _ -> 
            Group1 = lists:last(Group),
            Group2 = lists:nth(1, Group1),
            {Group2, State}        
    end.

get_nick(Table, User) ->
    LNick = ets:lookup(Table, User),
    TNick = lists:last(LNick),
    element(2, TNick).

send_iq_set_item(Jid, User, Group, Session) ->
    Iq = exmpp_client_roster:set_item(Jid, Group, User),
    exmpp_session:send_packet(Session, Iq).
