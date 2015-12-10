

# Module occi_category_mgr #
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

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#find-1">find/1</a></td><td></td></tr><tr><td valign="top"><a href="#find_all-0">find_all/0</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>
Starts the supervisor.</td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#load_schema-2">load_schema/2</a></td><td></td></tr><tr><td valign="top"><a href="#register_action-1">register_action/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_kind-1">register_kind/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_mixin-1">register_mixin/1</a></td><td></td></tr><tr><td valign="top"><a href="#unregister_mixin-1">unregister_mixin/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="find-1"></a>

### find/1 ###

<pre><code>
find(Uri::<a href="#type-occi_category">occi_category()</a> | <a href="#type-uri">uri()</a>) -&gt; [<a href="#type-occi_category">occi_category()</a>]
</code></pre>
<br />

<a name="find_all-0"></a>

### find_all/0 ###

<pre><code>
find_all() -&gt; {[<a href="#type-occi_kind">occi_kind()</a>], [<a href="#type-occi_mixin">occi_mixin()</a>], [<a href="#type-occi_action">occi_action()</a>]}
</code></pre>
<br />

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Occi_cid::<a href="#type-occi_cid">occi_cid()</a>) -&gt; {ok, <a href="#type-occi_category">occi_category()</a>} | {error, term()}
</code></pre>
<br />

<a name="hash-1"></a>

### hash/1 ###

<pre><code>
hash(Cid::<a href="#type-occi_cid">occi_cid()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

Starts the supervisor

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; ok
</code></pre>
<br />

<a name="load_schema-2"></a>

### load_schema/2 ###

<pre><code>
load_schema(Backend::atom(), Schema::<a href="#type-occi_schema">occi_schema()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="register_action-1"></a>

### register_action/1 ###

`register_action(Occi_action) -> any()`

<a name="register_kind-1"></a>

### register_kind/1 ###

`register_kind(Occi_kind) -> any()`

<a name="register_mixin-1"></a>

### register_mixin/1 ###

`register_mixin(Occi_mixin) -> any()`

<a name="unregister_mixin-1"></a>

### unregister_mixin/1 ###

`unregister_mixin(Occi_mixin) -> any()`

