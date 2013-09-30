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
render(#occi_kind{}=Kind, Sep) ->
    occi_renderer:join(
      occi_renderer:join([render_cid(Kind#occi_kind.id, Sep),
			  render_kv(<<"title">>, [Kind#occi_kind.title]),
			  render_kv(<<"rel">>, render_cid_uri(Kind#occi_kind.rel)),
			  render_kv(<<"attributes">>, lists:map(fun(X) -> render_attr_spec(X) end, Kind#occi_kind.attributes)),
			  render_kv(<<"actions">>, lists:map(fun(X) -> render_action_spec(X) end, Kind#occi_kind.actions)),
			  render_kv(<<"location">>, render_uri(Kind#occi_kind.location))],
			 <<"; ">>),
      Sep);
render(#occi_mixin{}=Mixin, Sep) ->
    occi_renderer:join(
      occi_renderer:join([render_cid(Mixin#occi_mixin.id, Sep),
			  render_kv(<<"title">>, [Mixin#occi_mixin.title]),
			  render_kv(<<"attributes">>, lists:map(fun(X) -> render_attr_spec(X) end, Mixin#occi_mixin.attributes)),
			  render_kv(<<"actions">>, lists:map(fun(X) -> render_action_spec(X) end, Mixin#occi_mixin.actions)),
			  render_kv(<<"location">>, render_uri(Mixin#occi_mixin.location))], 
			 <<"; ">>),
      Sep);
render(#occi_action_spec{}=Action, Sep) ->
    occi_renderer:join(
      occi_renderer:join([render_cid(Action#occi_action_spec.id, Sep),
			  render_kv(<<"title">>, [Action#occi_action_spec.title]),
			  render_kv(<<"attributes">>, lists:map(fun(X) -> render_attr_spec(X) end, Action#occi_action_spec.attributes))],
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

render_attr_spec(#occi_attr_spec{}=Attr) ->
    Ret = atom_to_list(Attr#occi_attr_spec.id),
    case render_attr_properties(Attr#occi_attr_spec.properties) of
	[] -> Ret;
	L -> [ Ret, L ]
    end.

render_attr_properties(undefined) ->
    [];
render_attr_properties(Properties) ->
    case lists:foldl(fun(required, Acc) ->
			      [atom_to_list(required) | Acc];
			 (immutable, Acc) ->
			      [atom_to_list(immutable) | Acc];
			 (_, Acc) ->
			      Acc
		      end, [], Properties) of
	[] -> [];
	L ->
	    [ <<"{">>, occi_renderer:join(L, <<",">>), <<"}">>]
    end.

render_action_spec(#occi_action_spec{}=Action) ->
    render_cid_uri(Action#occi_action_spec.id).

render_uri(Uri) ->
    occi_types:join_path([<<"">>|Uri]).

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
