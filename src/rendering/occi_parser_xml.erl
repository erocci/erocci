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
%%% @doc use EEP1108 format ofr JSON internal representation
%%% http://www.erlang.org/eeps/eep-0018.html
%%%
%%% @end
%%% Created :  7 Oct 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser_xml).
-compile({parse_transform, lager_transform}).

-behaviour(gen_fsm).

-include("occi.hrl").
-include("exmpp_xml.hrl").

%% API
-export([start_parser/0,
	 reset_parser/1,
	 stop_parser/1,
	 load/1,
	 parse/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% States
-export([init/3, extension/3, kind/3, eof/3, 
	 attribute_spec/3, mixin/3, action_spec/3]).

-define(SERVER, ?MODULE).
-define(PARSER_OPTIONS,
	[{engine, expat},
	 {names_as_atom, true},
	 {check_nss, xmpp},
	 {check_elems, xmpp},
	 {emit_endtag, true},
	 {root_depth, none},
	 {max_size, infinity}]).

-record(state, {
	  parser,
	  prefixes,
	  stack              :: [atom()],
	  extension          :: occi_extension(),
	  kind               :: reference(),
	  mixin              :: reference(),
	  attribute          :: occi_attr_spec()}).

-type(parser_id() :: reference()).
-type(parser_result() :: occi_extension() | {atom(), occi_object:id()}).

%%%===================================================================
%%% API
%%%===================================================================
load(Path) ->
    {ok, Bin} = file:read_file(Path),
    Parser = start_parser(),
    case catch parse(Parser, Bin) of
	{error, {parse_error, Reason}} ->
	    lager:error("Invalid XML file: ~p~n", [Reason]);
	{error, {'EXIT', Reason}} ->
	    lager:error("XML parser internal error: ~p~n", [Reason]);
	Result ->
	    lager:info("Succesfully loaded: ~p~n", [Result])
    end.

-spec parse(parser_id(), binary()) -> parser_result().
parse(Ref, Data) ->
    XP = gen_fsm:sync_send_all_state_event(Ref, get_parser),
    exmpp_xml:reset_parser(XP),
    case exmpp_xml:parse_final(XP, Data) of
	done ->
	    send_events(Ref, []);
	XMLElements ->
	    send_events(Ref, XMLElements)
    end.

start_parser() ->
    case gen_fsm:start(?MODULE, [], []) of
	{ok, Pid} -> Pid;
	Other -> 
	    lager:error("Error starting xml parser: ~p~n", [Other])
    end.

reset_parser(Ref) ->
    gen_fsm:send_all_state_event(Ref, stop).

stop_parser(Ref) ->
    gen_fsm:send_all_state_event(Ref, stop).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([]) ->
    XmlParser = exmpp_xml:start_parser(?PARSER_OPTIONS),
    {ok, init, #state{parser=XmlParser, stack=[init]}}.

-define(occi_ns, 'http://schemas.ogf.org/occi').
-define(xmlschema_ns, 'http://www.w3.org/2001/XMLSchema').

-define(extension, #xmlel{ns=?occi_ns, name=extension}).
-define(extensionEnd, #xmlendtag{ns=?occi_ns, name=extension}).

-define(categories, #xmlel{ns=?occi_ns, name=categories}).
-define(categoriesEnd, #xmlendtag{ns=?occi_ns, name=categories}).

-define(kind, #xmlel{ns=?occi_ns, name=kind}).
-define(kindEnd, #xmlendtag{ns=?occi_ns, name=kind}).

-define(mixin, #xmlel{ns=?occi_ns, name=mixin}).
-define(mixinEnd, #xmlendtag{ns=?occi_ns, name=mixin}).

-define(action, #xmlel{ns=?occi_ns, name=action}).
-define(actionEnd, #xmlendtag{ns=?occi_ns, name=action}).

-define(parent, #xmlel{ns=?occi_ns, name=parent}).
-define(parentEnd, #xmlendtag{ns=?occi_ns, name=parent}).

-define(depends, #xmlel{ns=?occi_ns, name=depends}).
-define(dependsEnd, #xmlendtag{ns=?occi_ns, name=depends}).

-define(applies, #xmlel{ns=?occi_ns, name=applies}).
-define(appliesEnd, #xmlendtag{ns=?occi_ns, name=applies}).

-define(attribute, #xmlel{ns=?occi_ns, name=attribute}).
-define(attributeEnd, #xmlendtag{ns=?occi_ns, name=attribute}).

-define(simpleType, #xmlel{ns=?xmlschema_ns, name=simpleType}).
-define(simpleTypeEnd, #xmlendtag{ns=?xmlschema_ns, name=simpleType}).

-define(simpleDef, #xmlel{ns=?xmlschema_ns}).
-define(simpleDefEnd, #xmlel{ns=?xmlschema_ns}).

init(E=?extension, _From, State) ->
    Name = to_atom(get_attr_value(E, <<"name">>)),
    Version = to_atom(get_attr_value(E, <<"version">>)),
    Ext = #occi_extension{name=Name,
			  version=Version},
    lager:info("Load extension: ~s v~s~n", [Name, Version]),
    push(extension, State#state{extension=Ext, prefixes=load_prefixes(E#xmlel.declared_ns)});
init(Event, _From, State) ->
    lager:error("Invalid node: ~p~n", [Event]),
    {reply, ok, init, State}.

extension(E=?kind, _From, #state{extension=Ext}=State) ->
    Scheme = to_atom(get_attr_value(E, <<"scheme">>, Ext#occi_extension.scheme)),
    Term = to_atom(get_attr_value(E, <<"term">>)),
    Kind = occi_kind:new({Scheme, Term}),
    lager:info("Load kind: ~s~s~n", [Scheme, Term]),
    push(kind, State#state{kind=Kind});
extension(_E=?mixin, _From, State) ->
    push(mixin, State);
extension(_E=?action, _From, State) ->
    push(action_spec, State);
extension(_E=?simpleType, _From, State) ->
    {reply, ok, extension, State};
extension(?extensionEnd, _From, #state{extension=Ext}=State) ->
    {reply, {eof, Ext}, eof, State};
extension(#xmlcdata{}, _From, State) ->
    {reply, ok, extension, State};
extension(Event, _From, State) ->
    Error = io_lib:format("Invalid element: ~p~n", [Event]),
    {reply, {error, {einval, Error}}, eof, State}.

kind(E=?parent, _From, #state{kind=Kind}=State) ->
    case occi_kind:set_parent(Kind, 
			      to_atom(get_attr_value(E, <<"scheme">>)),
			      to_atom(get_attr_value(E, <<"term">>))) of
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State};
	ok ->
	    {reply, ok, kind, State}
    end;
kind(_E=?parentEnd, _From, State) ->
    {reply, ok, kind, State};
kind(E=?attribute, _From, State) ->
    case make_attribute(E, State) of
	#occi_attr_spec{}=A -> 
	    push(attribute_spec, State#state{attribute=A});
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State}
    end;
kind(_E=?action, _From, State) ->
    push(action_spec, State);
kind(_E=?kindEnd, _From, State) ->
    pop(State#state{kind=undefined});
kind(#xmlcdata{}, _From, State) ->
    {reply, ok, kind, State};
kind(_E, _From, State) ->
    {reply, ok, kind, State}.

mixin(_E=?depends, _From, State) ->
    {reply, ok, mixin, State};
mixin(_E=?dependsEnd, _From, State) ->
    {reply, ok, mixin, State};
mixin(_E=?applies, _From, State) ->
    {reply, ok, mixin, State};
mixin(_E=?appliesEnd, _From, State) ->
    {reply, ok, mixin, State};
mixin(_E=?attribute, _From, State) ->
    push(attribute_spec, State);
mixin(_E=?action, _From, State) ->
    push(action_spec, State);
mixin(_E=?mixinEnd, _From, State) ->
    pop(State);
mixin(#xmlcdata{}, _From, State) ->
    {reply, ok, mixin, State};
mixin(_E, _From, State) ->
    {reply, ok, mixin, State}.

attribute_spec(_E=?attributeEnd, _From, State) ->
    pop(State#state{attribute=undefined});
attribute_spec(#xmlcdata{}, _From, State) ->
    {reply, ok, attribute_spec, State};
attribute_spec(_E, _From, State) ->
    {reply, ok, attribute_spec, State}.

action_spec(_E=?attribute, _From, State) ->
    push(attribute_spec, State);
action_spec(_E=?actionEnd, _From, State) ->
    pop(State);
action_spec(#xmlcdata{}, _From, State) ->
    {reply, ok, action_spec, State}.

eof(_Event, _From, State) ->
    {reply, ok, eof, State}.

%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%-------------------------------------------------------------------
handle_event(reset, _, #state{parser=Parser}=State) ->
    Parser2 = exmpp_xml:reset_parser(Parser),
    {next_state, init, State#state{parser=Parser2, stack=[init]}};
handle_event(stop, _, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(get_parser, _From, StateName, #state{parser=XP}=State) ->
    {reply, XP, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{parser=Parser}) ->
    exmpp_xml:stop_parser(Parser),
    ok.

%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_events(_Ref, []) ->
    throw({error, {parse_error, incomplete}});
send_events(Ref, [H|T]) ->
    lager:debug("Event: ~p~n", [H]),
    case catch gen_fsm:sync_send_event(Ref, H) of
	{'EXIT', Reason} ->
	    gen_fsm:send_all_state_event(Ref, reset),
	    throw({error, {'EXIT', Reason}});
	ok ->
	    send_events(Ref, T);
	{error, Reason} ->
	    gen_fsm:send_all_state_event(Ref, reset),
	    throw({error, {parse_error, Reason}});
	{eof, Result} ->
	    gen_fsm:send_all_state_event(Ref, reset),
	    Result
    end.

get_attr_value(XmlEl, Name) ->
    get_attr_value(XmlEl, Name, undefined).

get_attr_value(#xmlel{}=XmlEl, Name, Default) ->
    case exmpp_xml:get_attribute_node(XmlEl, Name) of
	undefined -> Default;
	Attr -> Attr#xmlattr.value
    end.

to_atom(Val) when is_binary(Val) ->
    list_to_atom(binary_to_list(Val));
to_atom(Val) when is_atom(Val) ->
    Val.

push(Next, #state{stack=undefined}=State) ->
    push(Next, State#state{stack=[]});
push(Next, #state{stack=Stack}=State) ->
    lager:debug("PUSH ~p~n", [Next]),
    {reply, ok, Next, State#state{stack=[Next|Stack]}}.

pop(#state{stack=[]}=State) ->
    lager:error("EMPTY STACK"),
    {reply, {error, stack_error}, eof, State};
pop(#state{stack=[Current,Previous|Stack]}=State) ->
    lager:debug("POP ~p~n", [Current]),
    {reply, ok, Previous, State#state{stack=[Previous|Stack]}}.

make_attribute(E, State) ->
    case get_attr_value(E, <<"name">>) of
	undefined ->
	    {error, "Undefined attribute name"};
	Name ->
	    #occi_attr_spec{id=Name, 
			    type=get_attr_type(get_attr_value(E, <<"type">>), State)}
    end.

% Return a dict with prefix->ns_as_atom key/value
-spec load_prefixes([{xmlname(), string()}]) -> term().
load_prefixes(NS) ->
    load_prefixes(NS, []).

load_prefixes([], Acc) ->
    dict:from_list(Acc);
load_prefixes([{Name, Prefix}|Tail], Acc) ->
    load_prefixes(Tail, [{Prefix, Name}|Acc]).

get_attr_type(undefined, _State) ->
    undefined;
get_attr_type(Type, #state{prefixes=P}) ->
    {Ns, Name} = resolve_ns(Type, P),
    get_attr_type({Ns, Name}).

resolve_ns(Bin, Dict) ->
    case string:tokens(binary_to_list(Bin), ":") of
	[Ns, Name] ->
	    case catch dict:fetch(Ns, Dict) of
		{badarg, _} ->
		    throw({error, {parse_error, unknown_ns}});
		Val ->
		    {Val, list_to_atom(Name)}
	    end;
	[_Name] ->
	    throw({error, {parse_error, unknown_ns}})
    end.

get_attr_type({?occi_ns, _}) ->
    string;
get_attr_type({?xmlschema_ns, integer}) ->
    integer;
get_attr_type({?xmlschema_ns, string}) ->
    string;
get_attr_type({?xmlschema_ns, float}) ->
    float;
get_attr_type({?xmlschema_ns, _}) ->
    string.
