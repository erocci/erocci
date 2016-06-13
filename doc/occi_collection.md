

# Module occi_collection #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-elem">elem()</a> ###


<pre><code>
elem() = {<a href="occi_uri.md#type-url">occi_uri:url()</a>, <a href="occi_entity.md#type-t">occi_entity:t()</a> | undefined}
</code></pre>




### <a name="type-id">id()</a> ###


<pre><code>
id() = binary() | <a href="occi_category.md#type-id">occi_category:id()</a>
</code></pre>




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#append-2">append/2</a></td><td>Append elements to the collection.</td></tr><tr><td valign="top"><a href="#elements-1">elements/1</a></td><td>Get all elements.</td></tr><tr><td valign="top"><a href="#elements-2">elements/2</a></td><td>Set elements or entities.</td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#from_map-2">from_map/2</a></td><td>Build collecton from AST.</td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Return collection id.</td></tr><tr><td valign="top"><a href="#ids-1">ids/1</a></td><td>Get all entity ids.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Create a new collection.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create a new collection.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new bounded collection.</td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td>Render collection into given mimetype.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>Collection size.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="append-2"></a>

### append/2 ###

<pre><code>
append(NewElements::<a href="ordsets.md#type-ordset">ordsets:ordset()</a>, Collection::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Append elements to the collection

<a name="elements-1"></a>

### elements/1 ###

<pre><code>
elements(Collection::<a href="#type-t">t()</a>) -&gt; <a href="ordsets.md#type-ordset">ordsets:ordset()</a>
</code></pre>
<br />

Get all elements

<a name="elements-2"></a>

### elements/2 ###

<pre><code>
elements(Elements::[<a href="#type-elem">elem()</a> | <a href="occi_entity.md#type-id">occi_entity:id()</a> | <a href="occi_entity.md#type-t">occi_entity:t()</a>], Collection::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Set elements or entities

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="from_map-2"></a>

### from_map/2 ###

<pre><code>
from_map(Id::<a href="occi_category.md#type-id">occi_category:id()</a> | binary(), Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Build collecton from AST

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(Collection::<a href="#type-t">t()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

Return collection id

<a name="ids-1"></a>

### ids/1 ###

<pre><code>
ids(Collection::<a href="#type-t">t()</a>) -&gt; []
</code></pre>
<br />

Get all entity ids

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Create a new collection

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Id::binary() | <a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Create a new collection.
If id is an uri, collection is unbounded.
If id is a category id, collection is bounded

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="occi_category.md#type-id">occi_category:id()</a>, Elements::[<a href="occi_entity.md#type-t">occi_entity:t()</a> | <a href="occi_entity.md#type-id">occi_entity:id()</a>]) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Creates a new bounded collection

<a name="render-3"></a>

### render/3 ###

<pre><code>
render(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, E::<a href="#type-t">t()</a>, Ctx::<a href="occi_uri.md#type-t">occi_uri:t()</a>) -&gt; iolist()
</code></pre>
<br />

Render collection into given mimetype

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Collection::<a href="#type-t">t()</a>) -&gt; integer()
</code></pre>
<br />

Collection size

