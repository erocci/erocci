

# Module occi_extension #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

An extension is a set of categories in the OCCI Core Model.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##

<a name="types"></a>

## Data Types ##




### <a name="type-id">id()</a> ###


<pre><code>
id() = binary()
</code></pre>




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_category-2">add_category/2</a></td><td>Add a category (kind or mixin) to the extension.</td></tr><tr><td valign="top"><a href="#add_import-2">add_import/2</a></td><td>Declare an extension to import.</td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>Load extension from ast.</td></tr><tr><td valign="top"><a href="#imports-1">imports/1</a></td><td>Get list of imports.</td></tr><tr><td valign="top"><a href="#kinds-1">kinds/1</a></td><td>Get the list of kinds of this extension.</td></tr><tr><td valign="top"><a href="#mixins-1">mixins/1</a></td><td>Get the list of mixins of this extension.</td></tr><tr><td valign="top"><a href="#name-1">name/1</a></td><td>Get the name of the extension.</td></tr><tr><td valign="top"><a href="#name-2">name/2</a></td><td>Set (optional) name of the extension.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates an extension with a given scheme.</td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td>Render extension into given mimetype.</td></tr><tr><td valign="top"><a href="#scheme-1">scheme/1</a></td><td>Get scheme of the extension.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_category-2"></a>

### add_category/2 ###

<pre><code>
add_category(Category::<a href="occi_category.md#type-t">occi_category:t()</a>, Extension::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Add a category (kind or mixin) to the extension.
Actions are contained within a kind or mixin.

<a name="add_import-2"></a>

### add_import/2 ###

<pre><code>
add_import(Scheme::binary() | string(), E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Declare an extension to import

WARNING: cycles are forbidden

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Load extension from ast

<a name="imports-1"></a>

### imports/1 ###

<pre><code>
imports(Extension::<a href="#type-t">t()</a>) -&gt; [<a href="occi_extension.md#type-id">occi_extension:id()</a>]
</code></pre>
<br />

Get list of imports

<a name="kinds-1"></a>

### kinds/1 ###

<pre><code>
kinds(Extension::<a href="#type-t">t()</a>) -&gt; [<a href="occi_category.md#type-t">occi_category:t()</a>]
</code></pre>
<br />

Get the list of kinds of this extension

<a name="mixins-1"></a>

### mixins/1 ###

<pre><code>
mixins(Extension::<a href="#type-t">t()</a>) -&gt; [<a href="occi_category.md#type-t">occi_category:t()</a>]
</code></pre>
<br />

Get the list of mixins of this extension

<a name="name-1"></a>

### name/1 ###

<pre><code>
name(Extension::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

Get the name of the extension.

<a name="name-2"></a>

### name/2 ###

<pre><code>
name(Name::binary(), Extension::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Set (optional) name of the extension

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Scheme::binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_uri, string() | binary()}`

Creates an extension with a given scheme.
The scheme is used as namespace for categories.

Throws extension if the scheme is not a valid URI.

<a name="render-3"></a>

### render/3 ###

<pre><code>
render(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, E::<a href="#type-t">t()</a>, Ctx::<a href="occi_uri.md#type-t">occi_uri:t()</a>) -&gt; iolist()
</code></pre>
<br />

Render extension into given mimetype

<a name="scheme-1"></a>

### scheme/1 ###

<pre><code>
scheme(Extension::<a href="#type-t">t()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

Get scheme of the extension

