

# Module occi_log_handler #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2015, Jean Parpaillon

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

__Behaviours:__ [`gen_event`](gen_event.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-2">handle_call/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_event-2">handle_event/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

<pre><code>
code_change(OldVsn::term() | {down, term()}, State::term(), Extra::term()) -&gt; {ok, NewState::term()}
</code></pre>
<br />

<a name="handle_call-2"></a>

### handle_call/2 ###

<pre><code>
handle_call(Request::term(), State::term()) -&gt; {ok, Reply::term(), NewState::term()} | {ok, Reply::term(), NewState::term(), hibernate} | {swap_handler, Reply::term(), Args1::term(), NewState::term(), Handler2::atom() | {atom(), term()}, Args2::term()} | {remove_handler, Reply::term()}
</code></pre>
<br />

<a name="handle_event-2"></a>

### handle_event/2 ###

<pre><code>
handle_event(Event::term(), State::term()) -&gt; {ok, NewState::term()} | {ok, NewState::term(), hibernate} | {swap_handler, Args1::term(), NewState::term(), Handler2::atom() | {atom(), term()}, Args2::term()} | remove_handler
</code></pre>
<br />

<a name="handle_info-2"></a>

### handle_info/2 ###

<pre><code>
handle_info(Info::term(), State::term()) -&gt; {ok, NewState::term()} | {ok, NewState::term(), hibernate} | {swap_handler, Args1::term(), NewState::term(), Handler2::atom() | {atom(), term()}, Args2::term()} | remove_handler
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Args::term()) -&gt; {ok, State::term()} | {ok, State::term(), hibernate} | {error, Reason::term()}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

<pre><code>
terminate(Arg::term(), State::term()) -&gt; term()
</code></pre>
<br />

