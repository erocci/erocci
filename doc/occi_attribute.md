

# Module occi_attribute #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = binary()
</code></pre>




### <a name="type-name_t">name_t()</a> ###


<pre><code>
name_t() = binary()
</code></pre>




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`




### <a name="type-type_t">type_t()</a> ###


<pre><code>
type_t() = <a href="occi_base_type.md#type-t">occi_base_type:t()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#category-1">category/1</a></td><td></td></tr><tr><td valign="top"><a href="#default-1">default/1</a></td><td></td></tr><tr><td valign="top"><a href="#default-2">default/2</a></td><td></td></tr><tr><td valign="top"><a href="#description-1">description/1</a></td><td></td></tr><tr><td valign="top"><a href="#description-2">description/2</a></td><td></td></tr><tr><td valign="top"><a href="#from_map-3">from_map/3</a></td><td></td></tr><tr><td valign="top"><a href="#mutable-1">mutable/1</a></td><td></td></tr><tr><td valign="top"><a href="#mutable-2">mutable/2</a></td><td></td></tr><tr><td valign="top"><a href="#name-1">name/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#pattern-1">pattern/1</a></td><td></td></tr><tr><td valign="top"><a href="#pattern-2">pattern/2</a></td><td></td></tr><tr><td valign="top"><a href="#required-1">required/1</a></td><td></td></tr><tr><td valign="top"><a href="#required-2">required/2</a></td><td></td></tr><tr><td valign="top"><a href="#title-1">title/1</a></td><td></td></tr><tr><td valign="top"><a href="#title-2">title/2</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="category-1"></a>

### category/1 ###

<pre><code>
category(A::<a href="#type-t">t()</a>) -&gt; <a href="occi_category.md#type-id">occi_category:id()</a>
</code></pre>
<br />

<a name="default-1"></a>

### default/1 ###

<pre><code>
default(A::<a href="#type-t">t()</a>) -&gt; <a href="occi_base_type.md#type-t">occi_base_type:t()</a>
</code></pre>
<br />

<a name="default-2"></a>

### default/2 ###

<pre><code>
default(Value::<a href="occi_base_type.md#type-t">occi_base_type:t()</a>, A::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="description-1"></a>

### description/1 ###

<pre><code>
description(A::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

<a name="description-2"></a>

### description/2 ###

<pre><code>
description(Desc::binary(), A::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="from_map-3"></a>

### from_map/3 ###

<pre><code>
from_map(Name::binary(), CatId::<a href="occi_category.md#type-id">occi_category:id()</a>, Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="mutable-1"></a>

### mutable/1 ###

<pre><code>
mutable(A::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="mutable-2"></a>

### mutable/2 ###

<pre><code>
mutable(Mutable::boolean(), A::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="name-1"></a>

### name/1 ###

<pre><code>
name(A::<a href="#type-t">t()</a>) -&gt; <a href="#type-name_t">name_t()</a>
</code></pre>
<br />

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Category::<a href="occi_category.md#type-id">occi_category:id()</a>, Name::<a href="#type-key">key()</a>, Type::<a href="#type-type_t">type_t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="pattern-1"></a>

### pattern/1 ###

<pre><code>
pattern(A::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

<a name="pattern-2"></a>

### pattern/2 ###

<pre><code>
pattern(Pattern::binary(), A::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="required-1"></a>

### required/1 ###

<pre><code>
required(A::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="required-2"></a>

### required/2 ###

<pre><code>
required(Required::boolean(), A::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="title-1"></a>

### title/1 ###

<pre><code>
title(A::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

<a name="title-2"></a>

### title/2 ###

<pre><code>
title(Title::binary(), A::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(A::<a href="#type-t">t()</a>) -&gt; <a href="#type-type_t">type_t()</a>
</code></pre>
<br />

