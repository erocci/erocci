

# Module occi_link #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#change_prefix-3">change_prefix/3</a></td><td>Change urls prefix.</td></tr><tr><td valign="top"><a href="#endpoint-2">endpoint/2</a></td><td>Make source / target urls relative to endpoint
URL are canonicalized: default ports are added to scheme if necessary
Throws <code>{invalid_link, binary()}</code> if source is outside of endpoint's domain.</td></tr><tr><td valign="top"><a href="#from_map-2">from_map/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Equivalent to <a href="#new-4"><tt>new(KindId, Src, Target, occi_resource:kind(Target))</tt></a>.</td></tr><tr><td valign="top"><a href="#new-5">new/5</a></td><td>Creates a new link.</td></tr><tr><td valign="top"><a href="#source-1">source/1</a></td><td></td></tr><tr><td valign="top"><a href="#target-1">target/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="change_prefix-3"></a>

### change_prefix/3 ###

<pre><code>
change_prefix(Op::<a href="occi_uri.md#type-prefix_op">occi_uri:prefix_op()</a>, Prefix::binary(), Link::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Change urls prefix

<a name="endpoint-2"></a>

### endpoint/2 ###

<pre><code>
endpoint(Endpoint::<a href="occi_uri.md#type-url">occi_uri:url()</a>, Link::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_link, binary()}`

Make source / target urls relative to endpoint
URL are canonicalized: default ports are added to scheme if necessary
Throws `{invalid_link, binary()}` if source is outside of endpoint's domain

<a name="from_map-2"></a>

### from_map/2 ###

<pre><code>
from_map(Kind::<a href="occi_kind.md#type-t">occi_kind:t()</a>, Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

`new(Kind) -> any()`

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(KindId::<a href="occi_category.md#type-id">occi_category:id()</a> | binary(), Src::binary() | <a href="occi_resource.md#type-t">occi_resource:t()</a>, Target::binary() | <a href="occi_resource.md#type-t">occi_resource:t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Equivalent to [`new(KindId, Src, Target, occi_resource:kind(Target))`](#new-4).

<a name="new-5"></a>

### new/5 ###

<pre><code>
new(KindId::<a href="occi_category.md#type-id">occi_category:id()</a> | binary(), Src::binary(), SrcKind::<a href="occi_category.md#type-id">occi_category:id()</a>, Target::binary(), TargetKind::<a href="occi_category.md#type-id">occi_category:id()</a> | undefined) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Creates a new link

<a name="source-1"></a>

### source/1 ###

<pre><code>
source(E::<a href="#type-t">t()</a>) -&gt; <a href="occi_uri.md#type-url">occi_uri:url()</a>
</code></pre>
<br />

<a name="target-1"></a>

### target/1 ###

<pre><code>
target(E::<a href="#type-t">t()</a>) -&gt; <a href="occi_uri.md#type-url">occi_uri:url()</a>
</code></pre>
<br />

