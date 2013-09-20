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
%%% Created : 10 Sep 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_renderer_json_tests).

-include_lib("eunit/include/eunit.hrl").

render1_test() ->
    Files = filelib:wildcard("../tests/erlang/valid*.cfg"),
    {setup,
     fun setup/0,
     fun cleanup/1,
     lists:flatten(lists:map(fun(File) ->
				     {ok, Terms} = file:consult(File),
				     lists:map(fun(Term) ->
						       ?_test(render(Term))
					       end, Terms)
			     end, Files))}.

render(Obj) ->
    occi_renderer_json:render(Obj).

parser_test_() ->
    Files = filelib:wildcard("../tests/json/valid*.json"),
    {setup,
     fun setup/0,
     fun cleanup/1,
     lists:map(fun(File) ->
		       ?_test(parse(File))
	       end, Files)}.

parse(File) ->
    ?debugFmt("parse ~s", [File]),
    {ok, In} = file:read_file(File),
    case  occi_renderer_json:parse(In) of
	{ok, _Result} ->
	    %?debugFmt("~n~p", [Result]),
	    ok;
	{error, {Pos, Error}} ->
	    throw({error, {Pos, Error}})
    end.

setup() ->
    ok.

cleanup(_) ->
    ok.
