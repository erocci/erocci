

# Module erocci_listener #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__This module defines the `erocci_listener` behaviour.__<br /> Required callback functions: `start_link/2`, `terminate/2`.

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-id">id()</a> ###


<pre><code>
id() = atom()
</code></pre>




### <a name="type-t">t()</a> ###


<pre><code>
t() = #listener{id = <a href="#type-id">id()</a>, handler = atom(), opts = term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Get listener ref.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create a listener structure.</td></tr><tr><td valign="top"><a href="#spec-1">spec/1</a></td><td>Get listener desc as child spec.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(Listener::<a href="#type-t">t()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

Get listener ref

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Else::{Ref::atom(), Handler::atom(), Opts::term()}) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{listener, term()}`

Create a listener structure

<a name="spec-1"></a>

### spec/1 ###

<pre><code>
spec(Listener::<a href="#type-t">t()</a>) -&gt; <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>
</code></pre>
<br />

Get listener desc as child spec

