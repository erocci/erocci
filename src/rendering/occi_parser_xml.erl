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
	 load_extension/1,
	 parse/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% States
-export([init/3, 
	 extension/3,
	 kind/3,
	 eof/3, 
	 attribute_spec/3,
	 mixin/3,
	 action_spec/3,
	 attr_type/3]).

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
	  action             :: reference(),
	  attribute          :: occi_attr(),
	  type}).

-type(parser_id() :: reference()).
-type(parser_result() :: occi_extension() | {atom(), occi_object:id()}).

%%%===================================================================
%%% API
%%%===================================================================
load_extension(Path) ->
    case occi_parser_xml:load(Path) of
	#occi_extension{}=Ext ->
	    lager:info("Loaded extension: ~s~n", [occi_extension:get_name(Ext)]),
	    Ext;
	{error, Reason} ->
	    lager:error("Error loading extension file ~s: ~p~n", [Path, Reason]),
	    {error, parse_error};
	{'EXIT', Reason} ->
	    lager:error("Error loading extension file ~s: ~p~n", [Path, Reason]),
	    {error, parse_error};
	Res ->
	    lager:error("Not an extension: ~p~n", [Res]),
	    throw({error, not_an_extension})
    end.

load(Path) ->
    {ok, Bin} = file:read_file(Path),
    Parser = start_parser(),
    case catch parse(Parser, Bin) of
	{error, {parse_error, Reason}} ->
	    lager:error("Invalid XML file: ~p~n", [Reason]),
	    {error, {parse_error, Reason}};
	{error, {'EXIT', Reason}} ->
	    lager:error("XML parser internal error: ~p~n", [Reason]),
	    throw({error, {'EXIT', Reason}});
	Result ->
	    Result
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
    {ok, init, #state{parser=XmlParser, 
		      stack=[init]}}.

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
-define(simpleDefEnd, #xmlendtag{ns=?xmlschema_ns}).

init(E=?extension, _From, State) ->
    Ext = make_extension(E, State),
    push(extension, State#state{extension=Ext, prefixes=load_prefixes(E#xmlel.declared_ns)});
init(Event, _From, State) ->
    lager:error("Invalid node: ~p~n", [Event]),
    {reply, ok, init, State}.

extension(E=?kind, _From, State) ->
    Kind = make_kind(E, State),
    push(kind, State#state{kind=Kind});
extension(E=?mixin, _From, State) ->
    Mixin = make_mixin(E, State),
    push(mixin, State#state{mixin=Mixin});
extension(E=?simpleType, _From, State) ->
    Id = to_atom(get_attr_value(E, <<"name">>)),
    push(attr_type, State#state{type=#occi_type{id=Id}});
extension(?extensionEnd, _From, #state{extension=Ext}=State) ->
    case catch occi_extension:check_types(Ext) of
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State};
	Ext2 ->
	    {reply, {eof, Ext2}, eof, State#state{extension=undefined}}
    end;
extension(#xmlcdata{}, _From, State) ->
    {reply, ok, extension, State};
extension(Event, _From, State) ->
    Error = io_lib:format("Invalid element: ~p~n", [Event]),
    {reply, {error, {einval, Error}}, eof, State}.

kind(E=?parent, _From, #state{kind=Kind}=State) ->
    case catch make_related(E, State) of
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State};
	{Scheme, Term} ->
	    occi_kind:set_parent(Kind, Scheme, Term),
	    {reply, ok, kind, State}
    end;
kind(_E=?parentEnd, _From, State) ->
    {reply, ok, kind, State};
kind(E=?attribute, _From, State) ->
    case catch make_attribute(E, State) of
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State};
	AttrSpec -> 
	    push(attribute_spec, State#state{attribute=AttrSpec})
    end;
kind(E=?action, _From, State) ->
    case catch make_action(E, State) of
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State};
	Action ->
	    push(action_spec, State#state{action=Action})
    end;
kind(_E=?kindEnd, _From, #state{extension=Ext, kind=Kind}=State) ->
    pop(State#state{kind=undefined, 
		    extension=occi_extension:add_kind(Ext, Kind)});
kind(#xmlcdata{}, _From, State) ->
    {reply, ok, kind, State};
kind(E, _From, State) ->
    Error = io_lib:format("Invalid element: ~p~n", [E]),
    {reply, {error, {einval, Error}}, eof, State}.

mixin(E=?depends, _From, #state{mixin=Mixin}=State) ->
    case catch make_related(E, State) of
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State};
	{Scheme, Term} ->
	    {reply, ok, mixin, 
	     State#state{mixin=occi_mixin:add_depends(Mixin, Scheme, Term)}}
    end;
mixin(_E=?dependsEnd, _From, State) ->
    {reply, ok, mixin, State};
mixin(E=?applies, _From, #state{mixin=Mixin}=State) ->
    case catch make_related(E, State) of
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State};
	{Scheme, Term} ->
	    {reply, ok, mixin, 
	     State#state{mixin=occi_mixin:add_applies(Mixin, Scheme, Term)}}
    end;
mixin(_E=?appliesEnd, _From, State) ->
    {reply, ok, mixin, State};
mixin(E=?attribute, _From, State) ->
    case catch make_attribute(E, State) of
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State};
	Attr -> 
	    push(attribute_spec, State#state{attribute=Attr})
    end;
mixin(E=?action, _From, State) ->
    case catch make_action(E, State) of
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State};
	Action ->
	    push(action_spec, State#state{action=Action})
    end;
mixin(_E=?mixinEnd, _From, #state{extension=Ext, mixin=Mixin}=State) ->
    pop(State#state{mixin=undefined,
		   extension=occi_extension:add_mixin(Ext, Mixin)});
mixin(#xmlcdata{}, _From, State) ->
    {reply, ok, mixin, State};
mixin(E, _From, State) ->
    Error = io_lib:format("Invalid element: ~p~n", [E]),
    {reply, {error, {einval, Error}}, eof, State}.

attribute_spec(_E=?attributeEnd, _From, 
	       #state{attribute=A, stack=[_Cur,Prev|_Stack]}=State) ->
    case occi_attribute:get_type_id(A) of
	undefined ->
	    {reply, {error, {undefined_type, occi_attribute:get_id(A)}}, eof, State};
	_ ->
	    State2 = case Prev of
			 kind -> 
			     Ref = State#state.kind,
			     State#state{kind=occi_kind:add_attribute(Ref, A)};
			 mixin -> 
			     Ref = State#state.mixin,
			     State#state{mixin=occi_mixin:add_attribute(Ref, A)};
			 action_spec -> 
			     Ref = State#state.action,
			     State#state{action=occi_action:add_attribute(Ref, A)}
		     end,
	    pop(State2#state{attribute=undefined})
	end;
attribute_spec(_E=?simpleType, _From, State) ->
    push(attr_type, State#state{type=#occi_type{}});
attribute_spec(#xmlcdata{}, _From, State) ->
    {reply, ok, attribute_spec, State};
attribute_spec(E, _From, State) ->
    Error = io_lib:format("Invalid element: ~p~n", [E]),
    {reply, {error, {einval, Error}}, eof, State}.

action_spec(E=?attribute, _From, State) ->
    case catch make_attribute(E, State) of
	{error, Reason} ->
	    {reply, {error, Reason}, eof, State};
	AttrSpec -> 
	    push(attribute_spec, State#state{attribute=AttrSpec})
    end;
action_spec(_E=?actionEnd, _From, 
	    #state{action=A, stack=[_Cur,Prev|_Stack]}=State) ->
    State2 = case Prev of
		 kind -> 
		     State#state{kind=occi_kind:add_action(State#state.kind, A)};
		 mixin -> 
		     State#state{mixin=occi_mixin:add_action(State#state.mixin, A)}
	     end,
    pop(State2#state{action=undefined});
action_spec(#xmlcdata{}, _From, State) ->
    {reply, ok, action_spec, State};
action_spec(E, _From, State) ->
    Error = io_lib:format("Invalid element: ~p~n", [E]),
    {reply, {error, {einval, Error}}, eof, State}.

attr_type(_E=?simpleTypeEnd, _From, 
	    #state{type=Type, extension=Ext, stack=[_Cur,Prev|_Stack]}=State) ->
    case Prev of
	extension ->
	    Ext2 = occi_extension:add_type(Ext, Type),
	    pop(State#state{type=undefined, extension=Ext2});
	attribute_spec ->
	    Attr = occi_attribute:set_type_id(State#state.attribute, 
					      get_attr_type(Type#occi_type.id)),
	    pop(State#state{type=undefined, attribute=Attr})
    end;
attr_type(_E=?simpleDef, _From, State) ->
    {reply, ok, attr_type, State};
attr_type(_E=?simpleDefEnd, _From, State) ->
    {reply, ok, attr_type, State};
attr_type(#xmlcdata{}, _From, State) ->
    {reply, ok, attr_type, State};
attr_type(E, _From, State) ->
    Error = io_lib:format("Invalid element: ~p~n", [E]),
    {reply, {error, {einval, Error}}, eof, State}.

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
    case get_attr_value(XmlEl, Name, undefined) of
	undefined ->
	    throw({error, {einval, 
			   io_lib:format("Missing attribute: ~s", [Name])}});
	Val -> Val
    end.

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
    {reply, ok, Next, State#state{stack=[Next|Stack]}}.

pop(#state{stack=[]}=State) ->
    {reply, {error, stack_error}, eof, State};
pop(#state{stack=[_Cur,Previous|Stack]}=State) ->
    {reply, ok, Previous, State#state{stack=[Previous|Stack]}}.

make_extension(E, _State) ->
    Name = to_atom(get_attr_value(E, <<"name">>, undefined)),
    Version = to_atom(get_attr_value(E, <<"version">>, undefined)),
    lager:info("Load extension: ~s v~s~n", [Name, Version]),
    occi_extension:new(Name, Version).

make_kind(E, #state{extension=Ext}) ->
    Scheme = to_atom(get_attr_value(E, <<"scheme">>, Ext#occi_extension.scheme)),
    Term = to_atom(get_attr_value(E, <<"term">>)),
    lager:info("Load kind: ~s~s~n", [Scheme, Term]),
    Kind = occi_kind:new({Scheme, Term}),
    Title = get_attr_value(E, <<"title">>, undefined),
    occi_kind:set_title(Kind, Title).

make_mixin(E, #state{extension=Ext}) ->
    Scheme = to_atom(get_attr_value(E, <<"scheme">>, Ext#occi_extension.scheme)),
    Term = to_atom(get_attr_value(E, <<"term">>)),
    lager:info("Load mixin: ~s~s~n", [Scheme, Term]),
    Mixin = occi_mixin:new({Scheme, Term}),
    Title = get_attr_value(E, <<"title">>, undefined),
    occi_mixin:set_title(Mixin, Title).

make_attribute(E, State) ->
    Name = get_attr_value(E, <<"name">>),
    Type = get_attr_type(get_attr_value(E, <<"type">>, undefined), 
			 State),
    lager:debug("Load attribute spec: ~s~n", [Name]),
    Attr = occi_attribute:new(Name),
    Attr2 = occi_attribute:set_type_id(Attr, Type),
    Title = get_attr_value(E, <<"title">>, undefined),
    occi_attribute:set_title(Attr2, Title).

make_action(E, _State) ->
    Scheme = to_atom(get_attr_value(E, <<"scheme">>)),
    Term = to_atom(get_attr_value(E, <<"term">>)),
    lager:debug("Load action spec: ~s~s~n", [Scheme, Term]),
    Action = occi_action:new({Scheme, Term}),
    Title = get_attr_value(E, <<"title">>, undefined),
    occi_action:set_title(Action, Title).

make_related(E, _State) ->
    {to_atom(get_attr_value(E, <<"scheme">>)),
     to_atom(get_attr_value(E, <<"term">>))}.

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
    string;
get_attr_type(undefined) ->
    string.
