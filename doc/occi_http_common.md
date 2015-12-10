

# Module occi_http_common #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#auth-2">auth/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_acl_op-1">get_acl_op/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_auth-0">get_auth/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_routes-1">get_routes/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_cors-2">set_cors/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_routes-2">set_routes/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="auth-2"></a>

### auth/2 ###

<pre><code>
auth(Ref::reference(), Req::term()) -&gt; {true, <a href="#type-occi_user">occi_user()</a>} | false
</code></pre>
<br />

<a name="get_acl_op-1"></a>

### get_acl_op/1 ###

<pre><code>
get_acl_op(Req::term()) -&gt; <a href="#type-acl_op">acl_op()</a>
</code></pre>
<br />

<a name="get_auth-0"></a>

### get_auth/0 ###

<pre><code>
get_auth() -&gt; iodata()
</code></pre>
<br />

<a name="get_routes-1"></a>

### get_routes/1 ###

<pre><code>
get_routes(Ref::atom()) -&gt; [term()]
</code></pre>
<br />

<a name="set_cors-2"></a>

### set_cors/2 ###

`set_cors(Req, Methods) -> any()`

<a name="set_routes-2"></a>

### set_routes/2 ###

<pre><code>
set_routes(Ref::atom(), Routes::list()) -&gt; ok
</code></pre>
<br />

<a name="start-3"></a>

### start/3 ###

`start(Ref, Protocol, Props) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(Ref) -> any()`

