

# Module occi_category #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-class">class()</a> ###


<pre><code>
class() = kind | mixin | action
</code></pre>




### <a name="type-id">id()</a> ###


<pre><code>
id() = {Scheme::binary(), Term::binary()}
</code></pre>




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#actions-1">actions/1</a></td><td></td></tr><tr><td valign="top"><a href="#add_action-2">add_action/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_attribute-2">add_attribute/2</a></td><td></td></tr><tr><td valign="top"><a href="#attribute-2">attribute/2</a></td><td></td></tr><tr><td valign="top"><a href="#attributes-1">attributes/1</a></td><td></td></tr><tr><td valign="top"><a href="#class-1">class/1</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#id_from_map-1">id_from_map/1</a></td><td>Return a category id from an AST.</td></tr><tr><td valign="top"><a href="#location-1">location/1</a></td><td></td></tr><tr><td valign="top"><a href="#location-2">location/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_id-1">parse_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td>Render category into given mimetype.</td></tr><tr><td valign="top"><a href="#title-1">title/1</a></td><td></td></tr><tr><td valign="top"><a href="#title-2">title/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="actions-1"></a>

### actions/1 ###

<pre><code>
actions(C::<a href="#type-t">t()</a>) -&gt; [<a href="occi_action.md#type-t">occi_action:t()</a>]
</code></pre>
<br />

<a name="add_action-2"></a>

### add_action/2 ###

<pre><code>
add_action(Action::<a href="occi_action.md#type-t">occi_action:t()</a>, C::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="add_attribute-2"></a>

### add_attribute/2 ###

<pre><code>
add_attribute(Attr::<a href="occi_attribute.md#type-t">occi_attribute:t()</a>, C::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="attribute-2"></a>

### attribute/2 ###

<pre><code>
attribute(Key::<a href="occi_attribute.md#type-key">occi_attribute:key()</a>, C::<a href="#type-t">t()</a>) -&gt; <a href="occi_attribute.md#type-t">occi_attribute:t()</a>
</code></pre>
<br />

<a name="attributes-1"></a>

### attributes/1 ###

<pre><code>
attributes(C::<a href="#type-t">t()</a>) -&gt; <a href="maps.md#type-map">maps:map()</a>
</code></pre>
<br />

<a name="class-1"></a>

### class/1 ###

<pre><code>
class(C::<a href="#type-t">t()</a>) -&gt; <a href="#type-class">class()</a>
</code></pre>
<br />

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(C::<a href="#type-t">t()</a>) -&gt; <a href="occi_category.md#type-id">occi_category:id()</a>
</code></pre>
<br />

<a name="id_from_map-1"></a>

### id_from_map/1 ###

<pre><code>
id_from_map(Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

Return a category id from an AST

<a name="location-1"></a>

### location/1 ###

<pre><code>
location(C::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

<a name="location-2"></a>

### location/2 ###

<pre><code>
location(Location::binary(), C::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::binary() | <a href="#type-id">id()</a>, Cls::<a href="#type-class">class()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_cid, term()}`

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Scheme::binary(), Term::binary(), Cls::<a href="#type-class">class()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="parse_id-1"></a>

### parse_id/1 ###

<pre><code>
parse_id(Id::string() | binary()) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

throws `{invalid_cid, term()}`

<a name="render-3"></a>

### render/3 ###

<pre><code>
render(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, E::<a href="#type-t">t()</a>, Ctx::<a href="occi_uri.md#type-t">occi_uri:t()</a>) -&gt; iolist()
</code></pre>
<br />

Render category into given mimetype

<a name="title-1"></a>

### title/1 ###

<pre><code>
title(C::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

<a name="title-2"></a>

### title/2 ###

<pre><code>
title(Title::binary(), C::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

