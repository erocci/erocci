%%%-------------------------------------------------------------------
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
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_text).
-compile({parse_transform, lager_transform}).

-include("occi.hrl").

%% API
-export([render/2]).

%%%===================================================================
%%% API
%%%===================================================================
render(#occi_category{id=#occi_cid{class=kind}=Id, location=Uri}=Kind, Sep) ->
    occi_renderer:join(
      occi_renderer:join([render_cid(Id, Sep),
			  render_kv(<<"title">>, [occi_kind:get_title(Kind)]),
			  render_kv(<<"rel">>, render_cid_uri(occi_kind:get_parent(Kind))),
			  render_kv(<<"attributes">>, render_attr_specs(occi_kind:get_attributes(Kind))),
			  render_kv(<<"actions">>, lists:map(fun(X) -> 
								     render_action_spec(X) 
							     end,
							     occi_kind:get_actions(Kind))),
			  render_kv(<<"location">>, Uri)],
			 <<"; ">>),
      Sep);

render(#occi_category{id=#occi_cid{class=mixin}=Id, location=Uri}=Mixin, Sep) ->
    occi_renderer:join(
      occi_renderer:join([render_cid(Id, Sep),
			  render_kv(<<"title">>, [occi_mixin:get_title(Mixin)]),
			  render_kv(<<"attributes">>, render_attr_specs(occi_mixin:get_attributes(Mixin))),
			  render_kv(<<"actions">>, lists:map(fun(X) -> 
								     render_action_spec(X)
							     end,
							     occi_mixin:get_actions(Mixin))),
			  render_kv(<<"location">>, Uri)], 
			 <<"; ">>),
      Sep);

render(#occi_action{id=Id}=Action, Sep) ->
    occi_renderer:join(
      occi_renderer:join([render_cid(Id, Sep),
			  render_kv(<<"title">>, [occi_action:get_title(Action)]),
			  render_kv(<<"attributes">>, render_attr_specs(occi_action:get_attributes(Action)))],
			 <<"; ">>),
      Sep);

render(O, _Sep) ->
    lager:error("Invalid value: ~p~n", [O]),
    throw({error, {occi_syntax, "invalid value"}}).

render_cid(#occi_cid{}=Cid, Sep) ->
    occi_renderer:join(
      occi_renderer:join([atom_to_list(Cid#occi_cid.term),
			  render_kv(<<"scheme">>, list_to_binary(atom_to_list(Cid#occi_cid.scheme))),
			  render_kv(<<"class">>, Cid#occi_cid.class) 
			 ], 
			 <<"; ">>), 
      Sep).

render_cid_uri(#occi_cid{}=Cid) ->
    [ atom_to_list(Cid#occi_cid.scheme), atom_to_list(Cid#occi_cid.term) ].

render_attr_specs(Attrs) ->
    lists:map(fun(Key) -> 
		      render_attr_spec(dict:fetch(Key, Attrs))
	      end, 
	      dict:fetch_keys(Attrs)).

render_attr_spec(#occi_attr{}=Attr) ->
    Ret = occi_attribute:get_id(Attr),
    case render_attr_properties(Attr) of
	[] -> Ret;
	L -> [ Ret, L ]
    end.

render_attr_properties(#occi_attr{}=A) ->
    L = case occi_attribute:is_required(A) of
	    true -> ["required"];		
	    false -> []
	end,
    L1 = lists:append(L, case occi_attribute:is_immutable(A) of
			     true -> ["immutable"];
			     false -> []
			 end),
    case L1 of
	[] -> [];
	L2 ->
	    [ <<"{">>, occi_renderer:join(L2, <<",">>), <<"}">>]
    end.

render_action_spec(#occi_action{}=Action) ->
    render_cid_uri(occi_action:get_id(Action)).

render_kv(_Key, undefined) ->
    [];
render_kv(_Key, <<>>) ->
    [];
render_kv(_Key, []) ->
    [];
render_kv(Key, Values) when is_list(Values) ->
    [Key, "=\"", occi_renderer:join(occi_renderer:to_list(Values), " "), "\""];
render_kv(Key, Value) when is_atom(Value) ->
    [Key, "=\"", atom_to_list(Value), "\""];
render_kv(Key, Value) ->
    [Key, "=\"", Value, "\""].
