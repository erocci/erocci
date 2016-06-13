

# Module occi_action #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Implements action definition (category)
For action invocation, @see occi_invoke.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#category-1">category/1</a></td><td>Get related category (kind or mixin).</td></tr><tr><td valign="top"><a href="#from_map-2">from_map/2</a></td><td>Load action from ast.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create new action category.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="category-1"></a>

### category/1 ###

<pre><code>
category(A::<a href="#type-t">t()</a>) -&gt; <a href="occi_category.md#type-id">occi_category:id()</a>
</code></pre>
<br />

Get related category (kind or mixin)

<a name="from_map-2"></a>

### from_map/2 ###

<pre><code>
from_map(Related::<a href="occi_category.md#type-id">occi_category:id()</a>, Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Load action from ast

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Scheme::binary(), Term::binary(), Related::<a href="occi_category.md#type-t">occi_category:t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Create new action category

