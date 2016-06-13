

# Module occi_entity #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Represents an OCCI entity.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
Uses maps for internal representation.
Type checking is achieved with check/1. When setting an attribute,
only attribute existence is checked.

<a name="types"></a>

## Data Types ##




### <a name="type-id">id()</a> ###


<pre><code>
id() = binary() | undefined
</code></pre>




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`




### <a name="type-validation">validation()</a> ###


<pre><code>
validation() = internal | client | server
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#actions-1">actions/1</a></td><td>Return list of action ids.</td></tr><tr><td valign="top"><a href="#add_mixin-2">add_mixin/2</a></td><td>Add a mixin to this entity
If an attribute is already defined, this mixin's definition take precedence over
the previous one.</td></tr><tr><td valign="top"><a href="#attributes-1">attributes/1</a></td><td>Return key-value attributes map
If attribute has default value, return the default value.</td></tr><tr><td valign="top"><a href="#change_prefix-3">change_prefix/3</a></td><td>Change prefix urls in entity.</td></tr><tr><td valign="top"><a href="#do-3">do/3</a></td><td>Equivalent to <a href="#do-4"><tt>do(occi_invoke:id(Invoke),
occi_invoke:attributes(Invoke), Fun, E)</tt></a>.</td></tr><tr><td valign="top"><a href="#do-4">do/4</a></td><td>Execute an action
<code>Fun = fun((ActionId :: occi_category:id(), Attributes :: maps:map(), Entity :: ()) -> {ok, t()} | {error, term()})</code></td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#from_map-2">from_map/2</a></td><td>New entity from AST.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td>opaque type representing an entity.</td></tr><tr><td valign="top"><a href="#id-2">id/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_subtype-2">is_subtype/2</a></td><td>Returns true if the entity is of type Type or subtype of.</td></tr><tr><td valign="top"><a href="#kind-1">kind/1</a></td><td></td></tr><tr><td valign="top"><a href="#location-1">location/1</a></td><td></td></tr><tr><td valign="top"><a href="#location-2">location/2</a></td><td></td></tr><tr><td valign="top"><a href="#merge_parents-2">merge_parents/2</a></td><td></td></tr><tr><td valign="top"><a href="#mixins-1">mixins/1</a></td><td></td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td>Render entity into given mimetype.</td></tr><tr><td valign="top"><a href="#rm_mixin-2">rm_mixin/2</a></td><td>Unassociate mixin from this entity
Attributes only defined by this mixin are removed.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Set the full list of attributes for this resource.</td></tr><tr><td valign="top"><a href="#update-3">update/3</a></td><td>Update attributes values.</td></tr><tr><td valign="top"><a href="#update_from_map-2">update_from_map/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="actions-1"></a>

### actions/1 ###

<pre><code>
actions(E::<a href="#type-t">t()</a>) -&gt; [<a href="occi_category.md#type-id">occi_category:id()</a>]
</code></pre>
<br />

Return list of action ids

<a name="add_mixin-2"></a>

### add_mixin/2 ###

<pre><code>
add_mixin(MixinId::<a href="occi_mixin.md#type-t">occi_mixin:t()</a> | <a href="occi_category.md#type-id">occi_category:id()</a> | string() | binary(), E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_category, [occi_category:id()](occi_category.md#type-id)}`

Add a mixin to this entity
If an attribute is already defined, this mixin's definition take precedence over
the previous one.

<a name="attributes-1"></a>

### attributes/1 ###

<pre><code>
attributes(E::<a href="#type-t">t()</a>) -&gt; #{}
</code></pre>
<br />

Return key-value attributes map
If attribute has default value, return the default value.
If attribute is not set and there is no default value, the attribute is not returned.

<a name="change_prefix-3"></a>

### change_prefix/3 ###

<pre><code>
change_prefix(Op::<a href="occi_uri.md#type-prefix_op">occi_uri:prefix_op()</a>, Prefix::binary(), Entity::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Change prefix urls in entity

<a name="do-3"></a>

### do/3 ###

<pre><code>
do(Invoke::<a href="occi_invoke.md#type-t">occi_invoke:t()</a>, Fun::function(), E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Equivalent to [`do(occi_invoke:id(Invoke),occi_invoke:attributes(Invoke), Fun, E)`](#do-4).

<a name="do-4"></a>

### do/4 ###

<pre><code>
do(ActionId::<a href="occi_categoy.md#type-id">occi_categoy:id()</a>, Attributes::<a href="maps.md#type-map">maps:map()</a>, Fun::function(), E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_action, [occi_category:id()](occi_category.md#type-id)}`

Execute an action
`Fun = fun((ActionId :: occi_category:id(), Attributes :: maps:map(), Entity :: ()) -> {ok, t()} | {error, term()})`

<a name="from_map-1"></a>

### from_map/1 ###

<pre><code>
from_map(Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="from_map-2"></a>

### from_map/2 ###

<pre><code>
from_map(Kind::<a href="occi_category.md#type-t">occi_category:t()</a> | binary(), Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

New entity from AST

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Key::<a href="occi_attribute.md#type-key">occi_attribute:key()</a>, E::<a href="#type-t">t()</a>) -&gt; <a href="occi_attribute.md#type-value">occi_attribute:value()</a> | undefined
</code></pre>
<br />

throws `{invalid_key, [occi_attribute:key()](occi_attribute.md#type-key)}`

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(E::<a href="#type-t">t()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

opaque type representing an entity

<a name="id-2"></a>

### id/2 ###

<pre><code>
id(Id::<a href="#type-id">id()</a>, E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="is_subtype-2"></a>

### is_subtype/2 ###

<pre><code>
is_subtype(X1::entity | resource | link, E::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

Returns true if the entity is of type Type or subtype of

<a name="kind-1"></a>

### kind/1 ###

<pre><code>
kind(E::<a href="#type-t">t()</a>) -&gt; <a href="occi_kind.md#type-id">occi_kind:id()</a>
</code></pre>
<br />

<a name="location-1"></a>

### location/1 ###

<pre><code>
location(E::<a href="#type-t">t()</a>) -&gt; <a href="occi_uri.md#type-url">occi_uri:url()</a> | undefined
</code></pre>
<br />

<a name="location-2"></a>

### location/2 ###

<pre><code>
location(Location::<a href="occi_uri.md#type-url">occi_uri:url()</a>, E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="merge_parents-2"></a>

### merge_parents/2 ###

`merge_parents(Kind, E) -> any()`

<a name="mixins-1"></a>

### mixins/1 ###

<pre><code>
mixins(E::<a href="#type-t">t()</a>) -&gt; [<a href="occi_mixin.md#type-id">occi_mixin:id()</a>]
</code></pre>
<br />

<a name="render-3"></a>

### render/3 ###

<pre><code>
render(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, E::<a href="#type-t">t()</a>, Ctx::<a href="uri.md#type-t">uri:t()</a>) -&gt; iolist()
</code></pre>
<br />

Render entity into given mimetype

<a name="rm_mixin-2"></a>

### rm_mixin/2 ###

<pre><code>
rm_mixin(Mixin::<a href="occi_mixin.md#type-t">occi_mixin:t()</a> | <a href="occi_category.md#type-id">occi_category:id()</a>, E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Unassociate mixin from this entity
Attributes only defined by this mixin are removed

<a name="set-3"></a>

### set/3 ###

<pre><code>
set(Attrs::#{}, Validation::<a href="#type-validation">validation()</a>, E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_keys, [[occi_attribute:key()](occi_attribute.md#type-key)]} | {invalid_value, [{[occi_attribute:key()](occi_attribute.md#type-key), [occi_base_type:t()](occi_base_type.md#type-t)}]} | {immutable, [[occi_attribute:key()](occi_attribute.md#type-key)]} | {required, [[occi_attribute:key()](occi_attribute.md#type-key)]}`

Set the full list of attributes for this resource

All required attributes must be set.

<a name="update-3"></a>

### update/3 ###

<pre><code>
update(Attrs::#{}, Validation::<a href="#type-validation">validation()</a>, E::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{invalid_keys, [[occi_attribute:key()](occi_attribute.md#type-key)]} | {invalid_value, [{[occi_attribute:key()](occi_attribute.md#type-key), [occi_base_type:t()](occi_base_type.md#type-t)}]} | {required, [[occi_attribute:key()](occi_attribute.md#type-key)]}`

Update attributes values.

<a name="update_from_map-2"></a>

### update_from_map/2 ###

<pre><code>
update_from_map(Map::<a href="occi_rendering.md#type-ast">occi_rendering:ast()</a>, Entity::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

