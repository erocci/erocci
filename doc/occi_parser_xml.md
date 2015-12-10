

# Module occi_parser_xml #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#action-3">action/3</a></td><td></td></tr><tr><td valign="top"><a href="#action_spec-3">action_spec/3</a></td><td></td></tr><tr><td valign="top"><a href="#attribute_spec-3">attribute_spec/3</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-4">code_change/4</a></td><td></td></tr><tr><td valign="top"><a href="#collection-3">collection/3</a></td><td></td></tr><tr><td valign="top"><a href="#eof-3">eof/3</a></td><td></td></tr><tr><td valign="top"><a href="#extension-3">extension/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_event-3">handle_event/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-3">handle_info/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_sync_event-4">handle_sync_event/4</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#kind-3">kind/3</a></td><td></td></tr><tr><td valign="top"><a href="#link-3">link/3</a></td><td></td></tr><tr><td valign="top"><a href="#load_extension-1">load_extension/1</a></td><td></td></tr><tr><td valign="top"><a href="#mixin-3">mixin/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_action-3">parse_action/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_collection-2">parse_collection/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_el-1">parse_el/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_entity-3">parse_entity/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_extension-1">parse_extension/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_full-1">parse_full/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_user_mixin-2">parse_user_mixin/2</a></td><td></td></tr><tr><td valign="top"><a href="#resource-3">resource/3</a></td><td></td></tr><tr><td valign="top"><a href="#restriction-3">restriction/3</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-3">terminate/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="action-3"></a>

### action/3 ###

`action(E, From, Parser) -> any()`

<a name="action_spec-3"></a>

### action_spec/3 ###

`action_spec(E, From, Parser) -> any()`

<a name="attribute_spec-3"></a>

### attribute_spec/3 ###

`attribute_spec(E, From, Parser) -> any()`

<a name="code_change-4"></a>

### code_change/4 ###

<pre><code>
code_change(OldVsn, StateName, State, Extra) -&gt; {ok, StateName, NewState}
</code></pre>
<br />

<a name="collection-3"></a>

### collection/3 ###

`collection(E, From, Parser) -> any()`

<a name="eof-3"></a>

### eof/3 ###

`eof(Event, From, State) -> any()`

<a name="extension-3"></a>

### extension/3 ###

`extension(E, From, Parser) -> any()`

<a name="handle_event-3"></a>

### handle_event/3 ###

<pre><code>
handle_event(Event, StateName, State) -&gt; {next_state, NextStateName, NextState} | {next_state, NextStateName, NextState, Timeout} | {stop, Reason, NewState}
</code></pre>
<br />

<a name="handle_info-3"></a>

### handle_info/3 ###

<pre><code>
handle_info(Info, StateName, State) -&gt; {next_state, NextStateName, NextState} | {next_state, NextStateName, NextState, Timeout} | {stop, Reason, NewState}
</code></pre>
<br />

<a name="handle_sync_event-4"></a>

### handle_sync_event/4 ###

<pre><code>
handle_sync_event(Event, From, StateName, State) -&gt; {next_state, NextStateName, NextState} | {next_state, NextStateName, NextState, Timeout} | {reply, Reply, NextStateName, NextState} | {reply, Reply, NextStateName, NextState, Timeout} | {stop, Reason, NewState} | {stop, Reason, Reply, NewState}
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

`init(Parser) -> any()`

<a name="init-3"></a>

### init/3 ###

`init(E, From, Parser) -> any()`

<a name="kind-3"></a>

### kind/3 ###

`kind(E, From, Parser) -> any()`

<a name="link-3"></a>

### link/3 ###

`link(E, From, Parser) -> any()`

<a name="load_extension-1"></a>

### load_extension/1 ###

`load_extension(Path) -> any()`

<a name="mixin-3"></a>

### mixin/3 ###

`mixin(E, From, Parser) -> any()`

<a name="parse_action-3"></a>

### parse_action/3 ###

`parse_action(Data, Env, Action) -> any()`

<a name="parse_collection-2"></a>

### parse_collection/2 ###

`parse_collection(Data, Env) -> any()`

<a name="parse_el-1"></a>

### parse_el/1 ###

`parse_el(Xmlel) -> any()`

<a name="parse_entity-3"></a>

### parse_entity/3 ###

`parse_entity(Data, Env, Occi_resource) -> any()`

<a name="parse_extension-1"></a>

### parse_extension/1 ###

`parse_extension(Data) -> any()`

<a name="parse_full-1"></a>

### parse_full/1 ###

`parse_full(Data) -> any()`

<a name="parse_user_mixin-2"></a>

### parse_user_mixin/2 ###

`parse_user_mixin(Data, Env) -> any()`

<a name="resource-3"></a>

### resource/3 ###

`resource(E, From, Parser) -> any()`

<a name="restriction-3"></a>

### restriction/3 ###

`restriction(E, From, Parser) -> any()`

<a name="terminate-3"></a>

### terminate/3 ###

<pre><code>
terminate(Reason, StateName, State) -&gt; <a href="#type-void">void()</a>
</code></pre>
<br />

