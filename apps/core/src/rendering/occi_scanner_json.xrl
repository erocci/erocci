% -*- mode: erlang -*-
Definitions.

White         = (\s|\t|\n)
Digit1to9     = [1-9]
Digit         = [0-9]
Digits        = {Digit}+
Int           = ({Digit}|{Digit1to9}{Digits}|-{Digit}|-{Digit1to9}{Digits})
Frac          = (\.{Digits})
Exp           = ({E}{Digits})
E             = [eE][+-]?
HexDigit      = [0-9a-f]
Float         = ({Int}{Frac}|{Int}{Exp}|{Int}{Frac}{Exp})
UnescapedChar = [\s!#\[\]\-~]
EscapedChar   = \\["\\bfnrt/]
UnicodeChar   = \\u{HexDigit}{HexDigit}{HexDigit}{HexDigit}
Char          = ({UnescapedChar}|{EscapedChar}|{UnicodeChar}|[a-zA-Z])
Chars         = {Char}+
DblQuote      = ["]

Rules.

{White}+                       : skip_token.
{DblQuote}{DblQuote}           : make_token(string, TokenLine, "").
{DblQuote}[^"]+{DblQuote}      : make_token(string, TokenLine, string:strip(TokenChars, both, $")).
{Int}                          : make_integer(TokenChars, TokenLine).
{Float}                        : make_float(TokenChars, TokenLine).
true                           : make_atom(true, TokenLine).
false                          : make_atom(false, TokenLine).
null                           : make_atom(null, TokenLine).
{                              : make_atom(objBegin, TokenLine).
}                              : make_atom(objEnd, TokenLine).
\[                             : make_atom(arrBegin, TokenLine).
\]                             : make_atom(arrEnd, TokenLine).
,                              : make_atom(comma, TokenLine).
:                              : make_atom(colon, TokenLine).

Erlang code.

-include("occi_log.hrl").
-include("occi_parser.hrl").

-export([start/1,
	 stop/1,
	 parse/2]).

-spec start(parser()) -> parser().
start(Sink) ->
    #parser{mod=?MODULE, sink=Sink}.

stop(_Parser) ->
    ok.

parse(Parser, Data) when is_list(Data)->
    case parse(Parser, [], Data) of
	{more, Rest} ->
	    {error, {trailing_chars, Rest}};
	Else ->
	    Else
    end;
parse(Parser, Data) when is_binary(Data) ->
    parse(Parser, binary_to_list(Data)).

parse(#parser{}=Parser, Cont, Data) ->
    case token(Cont, Data) of
	{more, Cont2} ->
	    {more, Cont2};
	{done, {ok, Token, _EndLine}, Rest} ->
	    case occi_parser:send_event(Token, ok, Parser) of
		{reply, {error, Err}, _, _} ->
		    {error, Err};
		ok ->
		    parse(Parser, [], Rest);
		{reply, {eof, Result}, _, _} ->
		    {ok, Result}
	    end;
	{done, {eof, _EndLine}, _Rest} ->
	    occi_parser:send_event(eof, ok, Parser);
	{done, Err, _Rest} ->
	    ?error("Error scanning json data at line~n"),
	    {error, Err}
    end.

%%%
%%% Priv
%%%
make_atom(Atom, Line) ->
    {token, #token{name=Atom, pos=Line}}.

make_token(Name, Line, Chars) ->
    {token, #token{name=Name, data=Chars, pos=Line}}.

make_float(Chars, Line) ->
    {token, #token{name=float, data=list_to_float(Chars), pos=Line}}.

make_integer(Chars, Line) ->
    {token, #token{name=integer, data=list_to_integer(Chars), pos=Line}}.
