

# Module occi_kind #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>Build kind from AST.</td></tr><tr><td valign="top"><a href="#has_parent-2">has_parent/2</a></td><td>Return true if kind has the parent.</td></tr><tr><td valign="top"><a href="#known_parent-1">known_parent/1</a></td><td>Return first known parent.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#parent-1">parent/1</a></td><td>Return parent of the category.</td></tr><tr><td valign="top"><a href="#parent-2">parent/2</a></td><td>Set parent of the category.</td></tr><tr><td valign="top"><a href="#parents-1">parents/1</a></td><td>Return all ancestors.</td></tr><tr><td valign="top"><a href="#parents-2">parents/2</a></td><td>Set full list of parents.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Build kind from AST

<a name="has_parent-2"></a>

### has_parent/2 ###

<pre><code>
has_parent(Parent::<a href="occi_category.md#type-id">occi_category:id()</a> | resource | link, Kind::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

Return true if kind has the parent

<a name="known_parent-1"></a>

### known_parent/1 ###

<pre><code>
known_parent(Kind::<a href="#type-t">t()</a>) -&gt; resource | entity
</code></pre>
<br />

Return first known parent

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Scheme::binary(), Term::binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="parent-1"></a>

### parent/1 ###

<pre><code>
parent(Kind::<a href="#type-t">t()</a>) -&gt; <a href="occi_category.md#type-id">occi_category:id()</a> | undefined
</code></pre>
<br />

Return parent of the category

<a name="parent-2"></a>

### parent/2 ###

<pre><code>
parent(Parent::binary() | <a href="occi_category.md#type-id">occi_category:id()</a>, Kind::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Set parent of the category

<a name="parents-1"></a>

### parents/1 ###

<pre><code>
parents(Kind::<a href="#type-t">t()</a>) -&gt; [<a href="occi_category.md#type-id">occi_category:id()</a>]
</code></pre>
<br />

Return all ancestors

<a name="parents-2"></a>

### parents/2 ###

<pre><code>
parents(Parents::[<a href="occi_category.md#type-id">occi_category:id()</a>], Kind::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Set full list of parents

