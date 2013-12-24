% -*- mode: erlang -*-
Definitions.

Digit1to9     = [1-9]
Digit         = [0-9]
Digits        = {Digit}+
Int           = {Digit}|{Digit1to9}{Digits}|-{Digit}|-{Digit1to9}{Digits}
Frac          = \.{Digits}
Exp           = {E}{Digits}
E             = [eE][+-]?
HexDigit      = [0-9a-f]
Float         = {Int}{Frac}|{Int}{Exp}|{Int}{Frac}{Exp}
UnescapedChar = [\s!#\[\]\-~]
EscapedChar   = \\["\\bfnrt/]
UnicodeChar   = \\u{HexDigit}{HexDigit}{HexDigit}{HexDigit}
Char          = {UnescapedChar}|{EscapedChar}|{UnicodeChar}|[a-zA-Z]
Chars         = {Char}+
DblQuote      = ["]
White         = (\s|\t|\n)

Rules.

{White}+                       : skip_token.
{DblQuote}{DblQuote}           : make_token(string, TokenLine, string:strip(TokenChars, both, $")).
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

-include("occi_parser.hrl").

make_atom(Atom, Line) ->
    {token, #token{name=Atom, pos=Line}}.

make_token(Name, Line, Chars) ->
    {token, #token{name=Name, data=Chars, pos=Line}}.

make_float(Chars, Line) ->
    {token, #token{name=float, data=list_to_float(Chars), pos=Line}}.

make_integer(Chars, Line) ->
    {token, #token{name=integer, data=list_to_integer(Chars), pos=Line}}.
