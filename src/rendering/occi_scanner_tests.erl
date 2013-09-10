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

-module(occi_scanner_tests).

-include_lib("eunit/include/eunit.hrl").

scanner_test_() ->
    Files = filelib:wildcard("../tests/plain/valid*"),
    {setup,
     fun setup/0,
     fun cleanup/1,
     lists:map(fun(File) ->
		       ?_test(scan(File))
	       end, Files)}.

scan(File) ->
    ?debugFmt("scan ~s", [File]),
    {ok, Input} = file:read_file(File),
    case occi_scanner:string(binary_to_list(Input)) of
	{ok,_Tokens,_EndLine} ->
	    %?debugFmt("~n~p", [Tokens]),
	    ok;
	ErrorInfo ->
	    throw(ErrorInfo)
    end.

setup() ->
    ok.

cleanup(_) ->
    ok.
