

# Module erocci_store #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) (c) 2013-2016 Jean Parpaillon

This file is provided to you under the license described
in the file LICENSE at the root of the project.

You can also download the LICENSE file from the following URL:
https://github.com/erocci/erocci/blob/master/LICENSE

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-data">data()</a> ###


<pre><code>
data() = {<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, iolist()}
</code></pre>




### <a name="type-error">error()</a> ###


<pre><code>
error() = not_found | method_not_allowed | forbidden | conflict | {unauthorized, binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#action-4">action/4</a></td><td>Execute an action on the given entity or collection.</td></tr><tr><td valign="top"><a href="#append_mixin-3">append_mixin/3</a></td><td>Associate entities to the given mixin.</td></tr><tr><td valign="top"><a href="#capabilities-2">capabilities/2</a></td><td>Retrieve capabilities node.</td></tr><tr><td valign="top"><a href="#collection-5">collection/5</a></td><td>Retrieve a bounded collection
<code>page</code> is an indexed integer that refers to a sub-collection of the requested.</td></tr><tr><td valign="top"><a href="#collections-0">collections/0</a></td><td>Return map of location -> bounded collections.</td></tr><tr><td valign="top"><a href="#create-4">create/4</a></td><td>Creates new entity.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>Delete entity.</td></tr><tr><td valign="top"><a href="#delete_all-2">delete_all/2</a></td><td>Delete all entities from bounded collection.</td></tr><tr><td valign="top"><a href="#delete_mixin-2">delete_mixin/2</a></td><td>Delete a user-defined mixin.</td></tr><tr><td valign="top"><a href="#get-5">get/5</a></td><td>Retrieve an entity or unbounded collection.</td></tr><tr><td valign="top"><a href="#new_mixin-2">new_mixin/2</a></td><td>Add a user-defined mixin.</td></tr><tr><td valign="top"><a href="#remove_mixin-3">remove_mixin/3</a></td><td>Disassociate entities from the given mixin.</td></tr><tr><td valign="top"><a href="#set_mixin-3">set_mixin/3</a></td><td>Replace collection of entities associated to mixin.</td></tr><tr><td valign="top"><a href="#update-3">update/3</a></td><td>Update entity.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="action-4"></a>

### action/4 ###

<pre><code>
action(Path::binary() | <a href="occi_category.md#type-t">occi_category:t()</a>, ActionTerm::binary(), X3::<a href="#type-data">data()</a>, Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>) -&gt; {ok, <a href="erocci_type.md#type-t">erocci_type:t()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Execute an action on the given entity or collection

<a name="append_mixin-3"></a>

### append_mixin/3 ###

<pre><code>
append_mixin(Mixin::<a href="occi_category.md#type-t">occi_category:t()</a>, X2::<a href="#type-data">data()</a>, Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>) -&gt; {ok, <a href="occi_collection.md#type-t">occi_collection:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Associate entities to the given mixin

<a name="capabilities-2"></a>

### capabilities/2 ###

<pre><code>
capabilities(Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>, Filter::<a href="erocci_filter.md#type-t">erocci_filter:t()</a> | undefined) -&gt; {ok, [<a href="occi_category.md#type-t">occi_category:t()</a>], <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Retrieve capabilities node

<a name="collection-5"></a>

### collection/5 ###

<pre><code>
collection(Category::<a href="occi_category.md#type-t">occi_category:t()</a>, Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>, Filter::<a href="erocci_filter.md#type-t">erocci_filter:t()</a>, Page::integer(), Number::integer() | undefined) -&gt; {ok, <a href="occi_collection.md#type-t">occi_collection:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Retrieve a bounded collection
`page` is an indexed integer that refers to a sub-collection of the requested.
`number` is an integer of items that SHOULD be contained in the collection.
If `number = undefined`, retrieve whole collection.

<a name="collections-0"></a>

### collections/0 ###

<pre><code>
collections() -&gt; <a href="maps.md#type-map">maps:map()</a>
</code></pre>
<br />

Return map of location -> bounded collections

<a name="create-4"></a>

### create/4 ###

<pre><code>
create(Path::<a href="occi_category.md#type-t">occi_category:t()</a> | binary(), Data::<a href="#type-data">data()</a>, Endpoint::binary(), Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>) -&gt; {ok, <a href="occi_entity.md#type-t">occi_entity:t()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Creates new entity

<a name="delete-2"></a>

### delete/2 ###

<pre><code>
delete(Path::binary(), Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>) -&gt; ok | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Delete entity

<a name="delete_all-2"></a>

### delete_all/2 ###

<pre><code>
delete_all(Category::<a href="occi_category.md#type-t">occi_category:t()</a>, Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>) -&gt; ok | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Delete all entities from bounded collection

<a name="delete_mixin-2"></a>

### delete_mixin/2 ###

<pre><code>
delete_mixin(X1::<a href="#type-data">data()</a>, Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>) -&gt; ok | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Delete a user-defined mixin

<a name="get-5"></a>

### get/5 ###

<pre><code>
get(Path::binary(), Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>, Filter::<a href="erocci_filter.md#type-t">erocci_filter:t()</a>, Start::integer(), Number::integer() | undefined) -&gt; {ok, <a href="occi_entity.md#type-t">occi_entity:t()</a> | <a href="occi_collection.md#type-t">occi_collection:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Retrieve an entity or unbounded collection

<a name="new_mixin-2"></a>

### new_mixin/2 ###

<pre><code>
new_mixin(X1::<a href="#type-data">data()</a>, Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>) -&gt; {ok, <a href="occi_mixin.md#type-t">occi_mixin:t()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Add a user-defined mixin

<a name="remove_mixin-3"></a>

### remove_mixin/3 ###

<pre><code>
remove_mixin(Mixin::<a href="occi_category.md#type-t">occi_category:t()</a>, X2::<a href="#type-data">data()</a>, Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>) -&gt; ok | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Disassociate entities from the given mixin

<a name="set_mixin-3"></a>

### set_mixin/3 ###

<pre><code>
set_mixin(Mixin::<a href="occi_category.md#type-t">occi_category:t()</a>, X2::<a href="#type-data">data()</a>, Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>) -&gt; {ok, <a href="occi_collection.md#type-t">occi_collection:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Replace collection of entities associated to mixin

<a name="update-3"></a>

### update/3 ###

<pre><code>
update(Path::binary(), Data::<a href="#type-data">data()</a>, Creds::<a href="erocci_creds.md#type-t">erocci_creds:t()</a>) -&gt; {ok, <a href="occi_entity.md#type-t">occi_entity:t()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Update entity

