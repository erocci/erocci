

# Module occi_parser_occi #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2014, Jean Parpaillon

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

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse-3">parse/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_action-3">parse_action/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_collection-2">parse_collection/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_entity-3">parse_entity/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_user_mixin-2">parse_user_mixin/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse-3"></a>

### parse/3 ###

<pre><code>
parse(Content::binary(), Ctx::term(), Type::<a href="#type-req_type">req_type()</a>) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="parse_action-3"></a>

### parse_action/3 ###

`parse_action(X1, Req, Action) -> any()`

<a name="parse_collection-2"></a>

### parse_collection/2 ###

`parse_collection(X1, Req) -> any()`

<a name="parse_entity-3"></a>

### parse_entity/3 ###

`parse_entity(X1, Req, Occi_resource) -> any()`

<a name="parse_user_mixin-2"></a>

### parse_user_mixin/2 ###

`parse_user_mixin(X1, Req) -> any()`

