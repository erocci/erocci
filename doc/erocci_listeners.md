

# Module erocci_listeners #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Sueprvisor for listeners (protocol handlers).

Copyright (c) (c) 2013-2016 Jean Parpaillon

This file is provided to you under the license described
in the file LICENSE at the root of the project.

You can also download the LICENSE file from the following URL:
https://github.com/erocci/erocci/blob/master/LICENSE

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-1">add/1</a></td><td>Add a new listener.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the supervisor.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-1"></a>

### add/1 ###

<pre><code>
add(L::<a href="occi_listener.md#type-t">occi_listener:t()</a>) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

Add a new listener

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>
<br />

Starts the supervisor

