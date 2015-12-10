

# Module occi_backend #
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

__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `occi_backend` behaviour.__<br /> Required callback functions: `init/1`, `terminate/1`, `update/2`, `save/2`, `delete/2`, `find/2`, `load/3`, `action/3`.

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#action-3">action/3</a></td><td></td></tr><tr><td valign="top"><a href="#cancel-2">cancel/2</a></td><td></td></tr><tr><td valign="top"><a href="#cast-3">cast/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td></td></tr><tr><td valign="top"><a href="#load-3">load/3</a></td><td></td></tr><tr><td valign="top"><a href="#save-2">save/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="action-3"></a>

### action/3 ###

`action(Ref, Id, Action) -> any()`

<a name="cancel-2"></a>

### cancel/2 ###

`cancel(Ref, Tag) -> any()`

<a name="cast-3"></a>

### cast/3 ###

`cast(Ref, Op, Req) -> any()`

<a name="delete-2"></a>

### delete/2 ###

`delete(Ref, Obj) -> any()`

<a name="find-2"></a>

### find/2 ###

`find(Ref, Request) -> any()`

<a name="load-3"></a>

### load/3 ###

`load(Ref, Request, Opts) -> any()`

<a name="save-2"></a>

### save/2 ###

`save(Ref, Obj) -> any()`

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Occi_backend::<a href="#type-occi_backend">occi_backend()</a>) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>
<br />

<a name="update-2"></a>

### update/2 ###

`update(Ref, Node) -> any()`

