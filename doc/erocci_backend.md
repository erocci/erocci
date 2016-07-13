

# Module erocci_backend #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (c) 2013-2016 Jean Parpaillon

This file is provided to you under the license described
in the file LICENSE at the root of the project.

You can also download the LICENSE file from the following URL:
https://github.com/erocci/erocci/blob/master/LICENSE

__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `erocci_backend` behaviour.__<br /> Required callback functions: `init/1`, `terminate/1`, `models/1`, `get/2`, `create/5`, `create/4`, `update/3`, `link/4`, `action/4`, `delete/2`, `mixin/4`, `unmixin/3`, `collection/5`.

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-backend_error">backend_error()</a> ###


<pre><code>
backend_error() = not_found | conflict | {internal, term()}
</code></pre>




### <a name="type-capability">capability()</a> ###


<pre><code>
capability() = undefined
</code></pre>




### <a name="type-error">error()</a> ###


<pre><code>
error() = <a href="#type-backend_error">backend_error()</a> | <a href="occi_rendering.md#type-error">occi_rendering:error()</a>
</code></pre>




### <a name="type-id">id()</a> ###


<pre><code>
id() = term()
</code></pre>




### <a name="type-t">t()</a> ###


<pre><code>
t() = #backend{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#action-3">action/3</a></td><td>Invoke an action on an existing entity.</td></tr><tr><td valign="top"><a href="#collection-5">collection/5</a></td><td>Retrieve a list of entities
start: integer > 0
number: integer > 0 | undefined (infinite).</td></tr><tr><td valign="top"><a href="#create-4">create/4</a></td><td>Create a new entity.</td></tr><tr><td valign="top"><a href="#default-0">default/0</a></td><td>Default backend.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>Delete an entity.</td></tr><tr><td valign="top"><a href="#depth-1">depth/1</a></td><td>Return mountpoint length.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Lookup for a node at Path.</td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_root-1">is_root/1</a></td><td>is root backend ?.</td></tr><tr><td valign="top"><a href="#link-4">link/4</a></td><td>Creates a link of type <code>Type</code> between resource and link id.</td></tr><tr><td valign="top"><a href="#mixin-4">mixin/4</a></td><td>Add a mixin to an existing entity.</td></tr><tr><td valign="top"><a href="#mnesia_disc_copies-1">mnesia_disc_copies/1</a></td><td>Get nodes on which a schema must exists.</td></tr><tr><td valign="top"><a href="#models-1">models/1</a></td><td>Get backend models.</td></tr><tr><td valign="top"><a href="#mountpoint-1">mountpoint/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates new backend entry.</td></tr><tr><td valign="top"><a href="#path-1">path/1</a></td><td></td></tr><tr><td valign="top"><a href="#spec-1">spec/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Start backend.</td></tr><tr><td valign="top"><a href="#unmixin-3">unmixin/3</a></td><td>Remove mixin from existing entity.</td></tr><tr><td valign="top"><a href="#update-3">update/3</a></td><td>Update an entity.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="action-3"></a>

### action/3 ###

<pre><code>
action(Backend::<a href="#type-t">t()</a>, Invoke::<a href="occi_invoke.md#type-t">occi_invoke:t()</a>, Entity::<a href="occi_entity.md#type-t">occi_entity:t()</a>) -&gt; {ok, <a href="occi_entity.md#type-t">occi_entity:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Invoke an action on an existing entity

<a name="collection-5"></a>

### collection/5 ###

<pre><code>
collection(Backend::<a href="#type-t">t()</a>, Id::<a href="occi_category.md#type-id">occi_category:id()</a> | binary(), Filter::<a href="erocci_filter.md#type-t">erocci_filter:t()</a>, Start::integer(), Number::integer() | undefined) -&gt; {ok, [<a href="occi_entity.md#type-location">occi_entity:location()</a>], <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Retrieve a list of entities
start: integer > 0
number: integer > 0 | undefined (infinite)

<a name="create-4"></a>

### create/4 ###

<pre><code>
create(B::<a href="#type-t">t()</a>, Entity::<a href="occi_entity.md#type-t">occi_entity:t()</a>, Owner::<a href="erocci_creds.md#type-user">erocci_creds:user()</a>, Group::<a href="erocci_creds.md#type-group">erocci_creds:group()</a>) -&gt; {ok, <a href="erocci_entity.md#type-t">erocci_entity:t()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Create a new entity

<a name="default-0"></a>

### default/0 ###

`default() -> any()`

Default backend

<a name="delete-2"></a>

### delete/2 ###

<pre><code>
delete(Backend::<a href="#type-t">t()</a>, Id::binary()) -&gt; ok | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Delete an entity

<a name="depth-1"></a>

### depth/1 ###

<pre><code>
depth(Backend::<a href="#type-t">t()</a>) -&gt; integer()
</code></pre>
<br />

Return mountpoint length

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Backend::<a href="#type-t">t()</a>, Location::binary()) -&gt; {ok, <a href="erocci_node.md#type-t">erocci_node:t()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Lookup for a node at Path

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(Backend::<a href="#type-t">t()</a>) -&gt; <a href="#type-id">id()</a>
</code></pre>
<br />

<a name="is_root-1"></a>

### is_root/1 ###

<pre><code>
is_root(Backend::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

is root backend ?

<a name="link-4"></a>

### link/4 ###

<pre><code>
link(Backend::<a href="#type-t">t()</a>, Resource::<a href="occi_resource.md#type-t">occi_resource:t()</a>, Type::source | target, LinkId::<a href="occi_link.md#type-location">occi_link:location()</a>) -&gt; ok | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Creates a link of type `Type` between resource and link id.

<a name="mixin-4"></a>

### mixin/4 ###

<pre><code>
mixin(Backend::<a href="#type-t">t()</a>, Entity::<a href="occi_entity.md#type-t">occi_entity:t()</a>, Mixin::<a href="occi_mixin.md#type-t">occi_mixin:t()</a>, Attributes::<a href="maps.md#type-map">maps:map()</a>) -&gt; {ok, <a href="occi_entity.md#type-t">occi_entity:t()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Add a mixin to an existing entity

<a name="mnesia_disc_copies-1"></a>

### mnesia_disc_copies/1 ###

<pre><code>
mnesia_disc_copies(Backend::<a href="#type-t">t()</a>) -&gt; [node()]
</code></pre>
<br />

Get nodes on which a schema must exists

<a name="models-1"></a>

### models/1 ###

<pre><code>
models(Backend::<a href="#type-t">t()</a>) -&gt; [<a href="occi_extension.md#type-t">occi_extension:t()</a>]
</code></pre>
<br />

Get backend models

<a name="mountpoint-1"></a>

### mountpoint/1 ###

<pre><code>
mountpoint(Backend::<a href="#type-t">t()</a>) -&gt; [binary()]
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Config::{Id::term(), Mod::atom(), Opts::term(), Mountpoint::binary() | string()}) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

throws `{backend, term()}`

Creates new backend entry

<a name="path-1"></a>

### path/1 ###

<pre><code>
path(Backend::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

<a name="spec-1"></a>

### spec/1 ###

<pre><code>
spec(Backend::<a href="#type-t">t()</a>) -&gt; <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>
</code></pre>
<br />

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Backend::<a href="#type-t">t()</a>) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>
<br />

Start backend

<a name="unmixin-3"></a>

### unmixin/3 ###

<pre><code>
unmixin(Backend::<a href="#type-t">t()</a>, Entity::<a href="occi_entity.md#type-t">occi_entity:t()</a>, Mixin::<a href="occi_mixin.md#type-t">occi_mixin:t()</a>) -&gt; {ok, <a href="occi_entity.md#type-t">occi_entity:t()</a>} | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Remove mixin from existing entity

<a name="update-3"></a>

### update/3 ###

<pre><code>
update(Backend::<a href="#type-t">t()</a>, Location::<a href="occi_entity.md#type-location">occi_entity:location()</a>, Attributes::<a href="maps.md#type-map">maps:map()</a>) -&gt; ok | {error, <a href="#type-error">error()</a>}
</code></pre>
<br />

Update an entity

