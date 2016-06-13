

# Module occi_dl #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Download resources, handling multiple urls for a single resource.

Copyright (c) (C) 2016, Jean Parpaillon

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
Queue requests for a same resource.

<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


<pre><code>
state() = <a href="ets.md#type-tid">ets:tid()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#resource-2">resource/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, S) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Evt, S) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(X1, S) -> any()`

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(X1::term()) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="resource-2"></a>

### resource/2 ###

<pre><code>
resource(Id::term(), Urls::[<a href="http_uri.md#type-uri">http_uri:uri()</a>]) -&gt; {ok, <a href="file.md#type-filename_all">file:filename_all()</a>} | {error, term()}
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, S) -> any()`

