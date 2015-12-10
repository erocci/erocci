

# Module occi_store #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

occi_store uses 2 structures to store mountpoints and associated
backends.

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

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##

These structures are stored in ets table:

{set, gb_set()} : a set of occi_node{type=mountpoint}. Used to quickly
iterate over mountpoints
{tree, gb_tree()} : a tree of {Length, [occi_node()]} where Length is
the length of '/' splitted path array.
The structure is used to get a backend given a path.

occi_store can return occi_store_err
- 500 : a internal server error happened
- 403 : the request is understood but the user is not allowed to execute this request
- 400 : bad request
<a name="types"></a>

## Data Types ##




### <a name="type-backend_desc">backend_desc()</a> ###


<pre><code>
backend_desc() = {<a href="#type-backend_ref">backend_ref()</a>, <a href="#type-backend_mod">backend_mod()</a>, <a href="#type-backend_opts">backend_opts()</a>, <a href="#type-backend_mountpoint">backend_mountpoint()</a>}
</code></pre>




### <a name="type-backend_mod">backend_mod()</a> ###


<pre><code>
backend_mod() = atom()
</code></pre>




### <a name="type-backend_mountpoint">backend_mountpoint()</a> ###


<pre><code>
backend_mountpoint() = binary() | string()
</code></pre>




### <a name="type-backend_opts">backend_opts()</a> ###


<pre><code>
backend_opts() = term()
</code></pre>




### <a name="type-backend_ref">backend_ref()</a> ###


<pre><code>
backend_ref() = atom()
</code></pre>




### <a name="type-occi_store_err">occi_store_err()</a> ###


<pre><code>
occi_store_err() = 500 | 403 | 400
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#action-3">action/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#find-1">find/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td></td></tr><tr><td valign="top"><a href="#load-3">load/3</a></td><td></td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td></td></tr><tr><td valign="top"><a href="#save-1">save/1</a></td><td></td></tr><tr><td valign="top"><a href="#save-2">save/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="action-3"></a>

### action/3 ###

`action(Occi_node, A, Ctx) -> any()`

<a name="delete-2"></a>

### delete/2 ###

<pre><code>
delete(Occi_node::<a href="#type-occi_node">occi_node()</a>, Ctx::<a href="#type-occi_store_ctx">occi_store_ctx()</a>) -&gt; ok | {error, <a href="#type-occi_store_err">occi_store_err()</a>}
</code></pre>
<br />

<a name="find-1"></a>

### find/1 ###

<pre><code>
find(Occi_node::<a href="#type-occi_node">occi_node()</a> | <a href="#type-occi_cid">occi_cid()</a>) -&gt; {ok, [<a href="#type-occi_node">occi_node()</a>]} | {error, term()}
</code></pre>
<br />

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Cid::<a href="#type-occi_cid">occi_cid()</a>) -&gt; {ok, <a href="#type-occi_category">occi_category()</a>} | {error, term()}
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="load-1"></a>

### load/1 ###

<pre><code>
load(Req::<a href="#type-occi_node">occi_node()</a>) -&gt; {ok, <a href="#type-occi_node">occi_node()</a>} | {error, <a href="#type-occi_store_err">occi_store_err()</a>}
</code></pre>
<br />

<a name="load-3"></a>

### load/3 ###

<pre><code>
load(Occi_node::<a href="#type-occi_node">occi_node()</a>, Opts::<a href="#type-occi_store_opts">occi_store_opts()</a>, Ctx::<a href="#type-occi_store_ctx">occi_store_ctx()</a>) -&gt; {ok, <a href="#type-occi_node">occi_node()</a>} | {error, <a href="#type-occi_store_err">occi_store_err()</a>}
</code></pre>
<br />

<a name="register-1"></a>

### register/1 ###

<pre><code>
register(X1::<a href="#type-backend_desc">backend_desc()</a>) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>
<br />

<a name="save-1"></a>

### save/1 ###

<pre><code>
save(Occi_node::<a href="#type-occi_node">occi_node()</a>) -&gt; ok | {error, <a href="#type-occi_store_err">occi_store_err()</a>}
</code></pre>
<br />

<a name="save-2"></a>

### save/2 ###

<pre><code>
save(Occi_node::<a href="#type-occi_node">occi_node()</a>, Occi_store_ctx::<a href="#type-occi_store_ctx">occi_store_ctx()</a>) -&gt; ok | {error, <a href="#type-occi_store_err">occi_store_err()</a>}
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>
<br />

Starts the server

<a name="update-2"></a>

### update/2 ###

<pre><code>
update(Occi_node::<a href="#type-occi_node">occi_node()</a>, Occi_store_ctx::<a href="#type-occi_store_ctx">occi_store_ctx()</a>) -&gt; ok | {error, <a href="#type-occi_store_err">occi_store_err()</a>}
</code></pre>
<br />

