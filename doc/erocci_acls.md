

# Module erocci_acls #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

See ACL format in `erocci_acl.hrl`.

Copyright (c) (c) 2014-2016 Jean Parpaillon

This file is provided to you under the license described
in the file LICENSE at the root of the project.

You can also download the LICENSE file from the following URL:
https://github.com/erocci/erocci/blob/master/LICENSE

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


<pre><code>
state() = [<a href="erocci_acl.md#type-t">erocci_acl:t()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#acls-0">acls/0</a></td><td>Retrieve all ACLs.</td></tr><tr><td valign="top"><a href="#check-3">check/3</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start ACLs manager.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="acls-0"></a>

### acls/0 ###

<pre><code>
acls() -&gt; [<a href="erocci_acl.md#type-t">erocci_acl:t()</a>]
</code></pre>
<br />

Retrieve all ACLs

<a name="check-3"></a>

### check/3 ###

<pre><code>
check(Op::<a href="#type-op">op()</a>, Node::<a href="occi_node.md#type-t">occi_node:t()</a>, Creds::<a href="occi_creds.md#type-t">occi_creds:t()</a>) -&gt; <a href="#type-policy">policy()</a>
</code></pre>
<br />

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, S, Extra) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Evt, From, S) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Evt, S) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, S) -> any()`

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(X1::[]) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>
<br />

Start ACLs manager

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, S) -> any()`

