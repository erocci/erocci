

# Module occi_authnz #
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

__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `occi_authnz` behaviour.__<br /> Required callback functions: `init/1`, `start/2`, `auth/2`, `share_group/3`, `create_group/2`, `delete_group/2`, `create_ingroup/4`, `delete_ingroup/2`, `update_group/3`, `update_user/3`, `create_user/3`, `delete_user/2`, `get_groups/1`, `get_group/2`, `get_users/1`, `get_user/2`, `get_ingroups/1`, `get_ingroup/2`, `get_group_user/2`.

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#auth-2">auth/2</a></td><td></td></tr><tr><td valign="top"><a href="#create_group-2">create_group/2</a></td><td></td></tr><tr><td valign="top"><a href="#create_ingroup-4">create_ingroup/4</a></td><td></td></tr><tr><td valign="top"><a href="#create_user-3">create_user/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete_group-2">delete_group/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_ingroup-2">delete_ingroup/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_user-2">delete_user/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_group-2">get_group/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_group_user-2">get_group_user/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_groups-1">get_groups/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_ingroup-2">get_ingroup/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_ingroups-1">get_ingroups/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_user-2">get_user/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_users-1">get_users/1</a></td><td></td></tr><tr><td valign="top"><a href="#share_group-3">share_group/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#update_group-3">update_group/3</a></td><td></td></tr><tr><td valign="top"><a href="#update_user-3">update_user/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="auth-2"></a>

### auth/2 ###

<pre><code>
auth(Ref::reference(), Credentials::term()) -&gt; true | false
</code></pre>
<br />

<a name="create_group-2"></a>

### create_group/2 ###

<pre><code>
create_group(Ref::reference(), Group::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="create_ingroup-4"></a>

### create_ingroup/4 ###

<pre><code>
create_ingroup(Ref::reference(), Idingroup::term(), User::term(), Group::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="create_user-3"></a>

### create_user/3 ###

<pre><code>
create_user(Ref::reference(), Jid::term(), User::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="delete_group-2"></a>

### delete_group/2 ###

<pre><code>
delete_group(Ref::reference(), Group::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="delete_ingroup-2"></a>

### delete_ingroup/2 ###

<pre><code>
delete_ingroup(Ref::reference(), InGroup::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="delete_user-2"></a>

### delete_user/2 ###

<pre><code>
delete_user(Ref::reference(), User::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="get_group-2"></a>

### get_group/2 ###

<pre><code>
get_group(Ref::reference(), Group::term()) -&gt; term() | {error, term()}
</code></pre>
<br />

<a name="get_group_user-2"></a>

### get_group_user/2 ###

<pre><code>
get_group_user(Ref::reference(), User::term()) -&gt; term()
</code></pre>
<br />

<a name="get_groups-1"></a>

### get_groups/1 ###

<pre><code>
get_groups(Ref::reference()) -&gt; term()
</code></pre>
<br />

<a name="get_ingroup-2"></a>

### get_ingroup/2 ###

<pre><code>
get_ingroup(Ref::reference(), InGroup::term()) -&gt; term() | {error, term()}
</code></pre>
<br />

<a name="get_ingroups-1"></a>

### get_ingroups/1 ###

<pre><code>
get_ingroups(Ref::reference()) -&gt; term()
</code></pre>
<br />

<a name="get_user-2"></a>

### get_user/2 ###

<pre><code>
get_user(Ref::reference(), User::term()) -&gt; term() | {error, term()}
</code></pre>
<br />

<a name="get_users-1"></a>

### get_users/1 ###

<pre><code>
get_users(Ref::reference()) -&gt; term()
</code></pre>
<br />

<a name="share_group-3"></a>

### share_group/3 ###

<pre><code>
share_group(Ref::reference(), User1::term(), User2::term()) -&gt; true | false
</code></pre>
<br />

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(Ref::reference(), Opts::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Mod::atom(), Opts::term()) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>
<br />

Starts the server

<a name="update_group-3"></a>

### update_group/3 ###

<pre><code>
update_group(Ref::reference(), Group::term(), NGroup::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="update_user-3"></a>

### update_user/3 ###

<pre><code>
update_user(Ref::reference(), Jid::term(), User::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

