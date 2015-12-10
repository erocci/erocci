

# Module occi_parser_json0 #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

An event based JSON parser.

Copyright (c) (C) 2013, Jean Parpaillon

This file is provided to you under the Apache License,
Version 2.0 (the "License"); you may not use this file
except in compliance with the License.  You may obtain
a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.

__Behaviours:__ [`gen_fsm`](gen_fsm.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#array-3">array/3</a></td><td></td></tr><tr><td valign="top"><a href="#elements-3">elements/3</a></td><td></td></tr><tr><td valign="top"><a href="#eof-3">eof/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#members-3">members/3</a></td><td></td></tr><tr><td valign="top"><a href="#object-3">object/3</a></td><td></td></tr><tr><td valign="top"><a href="#pair-3">pair/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#value-3">value/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="array-3"></a>

### array/3 ###

`array(Token, From, Parser) -> any()`

<a name="elements-3"></a>

### elements/3 ###

`elements(Token, From, Ctx) -> any()`

<a name="eof-3"></a>

### eof/3 ###

`eof(E, F, Ctx) -> any()`

<a name="init-3"></a>

### init/3 ###

`init(Token, From, Ctx) -> any()`

<a name="members-3"></a>

### members/3 ###

`members(Token, From, Ctx) -> any()`

<a name="object-3"></a>

### object/3 ###

`object(Token, From, Parser) -> any()`

<a name="pair-3"></a>

### pair/3 ###

`pair(Token, From, Ctx) -> any()`

<a name="parse-2"></a>

### parse/2 ###

<pre><code>
parse(Parser::<a href="#type-parser">parser()</a>, Data::binary()) -&gt; <a href="#type-parser_result">parser_result()</a>
</code></pre>
<br />

<a name="start-1"></a>

### start/1 ###

`start(Sink) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(Parser) -> any()`

<a name="value-3"></a>

### value/3 ###

`value(Token, From, Ctx) -> any()`

