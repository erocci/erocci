

# Module occi_invoke #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Implements action invocation.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#attributes-1">attributes/1</a></td><td>Get invocation attributes.</td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>Load an action invocation from AST.</td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Return action id.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new action invocation.</td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td>Render action invocation into given mimetype.</td></tr><tr><td valign="top"><a href="#title-1">title/1</a></td><td>Return action title.</td></tr><tr><td valign="top"><a href="#title-2">title/2</a></td><td>Set action title.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="attributes-1"></a>

### attributes/1 ###

<pre><code>
attributes(Invoke::<a href="#type-t">t()</a>) -&gt; <a href="maps.md#type-map">maps:map()</a>
</code></pre>
<br />

Get invocation attributes

<a name="from_map-1"></a>

### from_map/1 ###

`from_map(Map) -> any()`

Load an action invocation from AST

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(Invoke::<a href="#type-t">t()</a>) -&gt; <a href="occi_category.md#type-id">occi_category:id()</a>
</code></pre>
<br />

Return action id

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="occi_category.md#type-id">occi_category:id()</a>, Attributes::<a href="maps.md#type-map">maps:map()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Creates a new action invocation

<a name="render-3"></a>

### render/3 ###

<pre><code>
render(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, I::<a href="#type-t">t()</a>, Ctx::<a href="occi_uri.md#type-t">occi_uri:t()</a>) -&gt; iolist()
</code></pre>
<br />

Render action invocation into given mimetype

<a name="title-1"></a>

### title/1 ###

<pre><code>
title(Invoke::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

Return action title

<a name="title-2"></a>

### title/2 ###

<pre><code>
title(Title::binary(), Invoke::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Set action title

