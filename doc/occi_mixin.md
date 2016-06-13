

# Module occi_mixin #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_apply-2">add_apply/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_depend-2">add_depend/2</a></td><td></td></tr><tr><td valign="top"><a href="#applies-1">applies/1</a></td><td></td></tr><tr><td valign="top"><a href="#depends-1">depends/1</a></td><td></td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>Load mixin from an AST.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#tag-1">tag/1</a></td><td></td></tr><tr><td valign="top"><a href="#tag-2">tag/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_apply-2"></a>

### add_apply/2 ###

<pre><code>
add_apply(Apply::binary() | <a href="occi_category.md#type-id">occi_category:id()</a>, Mixin::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="add_depend-2"></a>

### add_depend/2 ###

<pre><code>
add_depend(Depend::binary() | <a href="occi_category.md#type-id">occi_category:id()</a>, Mixin::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="applies-1"></a>

### applies/1 ###

<pre><code>
applies(M::<a href="#type-t">t()</a>) -&gt; [<a href="occi_category.md#type-id">occi_category:id()</a>]
</code></pre>
<br />

<a name="depends-1"></a>

### depends/1 ###

<pre><code>
depends(M::<a href="#type-t">t()</a>) -&gt; [<a href="occi_category.md#type-id">occi_category:id()</a>]
</code></pre>
<br />

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Load mixin from an AST

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Scheme::binary(), Term::binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="tag-1"></a>

### tag/1 ###

<pre><code>
tag(M::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="tag-2"></a>

### tag/2 ###

<pre><code>
tag(IsTag::boolean(), M::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

