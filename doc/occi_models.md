

# Module occi_models #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

The module is used to access the full model of an OCCI endpoint,
ie, more or less extensions with resolved imports and location associated
to each kind or mixin.

Copyright (c) (C) 2016, Jean Parpaillon

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#action-1">action/1</a></td><td>Return an action definition.</td></tr><tr><td valign="top"><a href="#add_category-1">add_category/1</a></td><td>Add category to model and returns the stored category,
eventually with location set.</td></tr><tr><td valign="top"><a href="#attribute-2">attribute/2</a></td><td></td></tr><tr><td valign="top"><a href="#attributes-1">attributes/1</a></td><td></td></tr><tr><td valign="top"><a href="#categories-0">categories/0</a></td><td>Return the list of categories.</td></tr><tr><td valign="top"><a href="#category-1">category/1</a></td><td>Return a category.</td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#import-1">import/1</a></td><td>Import an extension into the model.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#init_mnesia-0">init_mnesia/0</a></td><td></td></tr><tr><td valign="top"><a href="#kind-1">kind/1</a></td><td>Return a kind.</td></tr><tr><td valign="top"><a href="#kind-2">kind/2</a></td><td>Return a kind, checking it has specified parent.</td></tr><tr><td valign="top"><a href="#location-1">location/1</a></td><td>If any, return a category given a location.</td></tr><tr><td valign="top"><a href="#rm_category-1">rm_category/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="action-1"></a>

### action/1 ###

<pre><code>
action(ActionId::<a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; <a href="occi_action.md#type-t">occi_action:t()</a>
</code></pre>
<br />

Return an action definition

<a name="add_category-1"></a>

### add_category/1 ###

<pre><code>
add_category(Cat::<a href="occi_category.md#type-t">occi_category:t()</a>) -&gt; <a href="occi_category.md#type-t">occi_category:t()</a>
</code></pre>
<br />

Add category to model and returns the stored category,
eventually with location set

<a name="attribute-2"></a>

### attribute/2 ###

<pre><code>
attribute(Key::<a href="occi_attribute.md#type-key">occi_attribute:key()</a>, Others::[<a href="occi_category.md#type-id">occi_category:id()</a>]) -&gt; <a href="occi_attribute.md#type-t">occi_attribute:t()</a> | undefined
</code></pre>
<br />

throws `{unknown_category, term()}`

<a name="attributes-1"></a>

### attributes/1 ###

<pre><code>
attributes(CatId::<a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; [<a href="occi_attribute.md#type-t">occi_attribute:t()</a>]
</code></pre>
<br />

throws `{unknown_category, [occi_category:id()](occi_category.md#type-id)}`

<a name="categories-0"></a>

### categories/0 ###

<pre><code>
categories() -&gt; [<a href="occi_category.md#type-t">occi_category:t()</a>]
</code></pre>
<br />

Return the list of categories

<a name="category-1"></a>

### category/1 ###

<pre><code>
category(Id::binary() | <a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; <a href="occi_category.md#type-t">occi_category:t()</a> | undefined
</code></pre>
<br />

Return a category

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, S, Extra) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Call, From, S) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Evt, S) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, S) -> any()`

<a name="import-1"></a>

### import/1 ###

<pre><code>
import(E::<a href="occi_extension.md#type-t">occi_extension:t()</a>) -&gt; {ok, [<a href="occi_category.md#type-t">occi_category:t()</a>]} | {error, term()}
</code></pre>
<br />

Import an extension into the model

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="init_mnesia-0"></a>

### init_mnesia/0 ###

`init_mnesia() -> any()`

<a name="kind-1"></a>

### kind/1 ###

<pre><code>
kind(KindId::<a href="occi_category.md#type-id">occi_category:id()</a> | binary()) -&gt; <a href="occi_category.md#type-id">occi_category:id()</a>
</code></pre>
<br />

throws `{unknown_category, [occi_category:id()](occi_category.md#type-id)}`

Return a kind

<a name="kind-2"></a>

### kind/2 ###

<pre><code>
kind(Parent::link | resource, KindId::<a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; <a href="occi_category.md#type-t">occi_category:t()</a>
</code></pre>
<br />

throws `{unknown_category, [occi_category:id()](occi_category.md#type-id)} | {invalid_kind, [occi_category:id()](occi_category.md#type-id)}`

Return a kind, checking it has specified parent

<a name="location-1"></a>

### location/1 ###

<pre><code>
location(Path::binary()) -&gt; <a href="occi_category.md#type-t">occi_category:t()</a> | undefined
</code></pre>
<br />

If any, return a category given a location.
Otherwise, returns 'undefined

<a name="rm_category-1"></a>

### rm_category/1 ###

<pre><code>
rm_category(Id::<a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; ok
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, S) -> any()`

