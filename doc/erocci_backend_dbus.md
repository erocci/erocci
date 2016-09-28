

# Module erocci_backend_dbus #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (c) 2013-2016 Jean Parpaillon

This file is provided to you under the license described
in the file LICENSE at the root of the project.

You can also download the LICENSE file from the following URL:
https://github.com/erocci/erocci/blob/master/LICENSE

__Behaviours:__ [`erocci_backend`](erocci_backend.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#action-4">action/4</a></td><td></td></tr><tr><td valign="top"><a href="#collection-5">collection/5</a></td><td></td></tr><tr><td valign="top"><a href="#create-4">create/4</a></td><td></td></tr><tr><td valign="top"><a href="#create-5">create/5</a></td><td></td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#link-4">link/4</a></td><td></td></tr><tr><td valign="top"><a href="#mixin-4">mixin/4</a></td><td></td></tr><tr><td valign="top"><a href="#models-1">models/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-1">terminate/1</a></td><td></td></tr><tr><td valign="top"><a href="#unlink-4">unlink/4</a></td><td></td></tr><tr><td valign="top"><a href="#unmixin-3">unmixin/3</a></td><td></td></tr><tr><td valign="top"><a href="#update-3">update/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="action-4"></a>

### action/4 ###

<pre><code>
action(Location::<a href="occi_uri.md#type-url">occi_uri:url()</a>, ActionId::<a href="occi_action.md#type-id">occi_action:id()</a>, Attributes::<a href="maps.md#type-map">maps:map()</a>, State::term()) -&gt; {{ok, <a href="occi_entity.md#type-t">occi_entity:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="error_backend.md#type-error">error_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="collection-5"></a>

### collection/5 ###

<pre><code>
collection(Id::<a href="occi_category.md#type-id">occi_category:id()</a> | binary(), Filter::<a href="erocci_filter.md#type-t">erocci_filter:t()</a>, Start::integer(), Number::integer() | undefined, State::term()) -&gt; {{ok, [<a href="occi_entity.md#type-id">occi_entity:id()</a>], <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="create-4"></a>

### create/4 ###

<pre><code>
create(Entity::<a href="occi_entity.md#type-t">occi_entity:t()</a>, Owner::<a href="erocci_creds.md#type-user">erocci_creds:user()</a>, Group::<a href="erocci_creds.md#type-group">erocci_creds:group()</a>, State::term()) -&gt; {{ok, <a href="occi_uri.md#type-url">occi_uri:url()</a>, <a href="occi_entity.md#type-t">occi_entity:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="create-5"></a>

### create/5 ###

<pre><code>
create(Location::binary(), Entity::<a href="occi_entity.md#type-t">occi_entity:t()</a>, Owner::<a href="erocci_creds.md#type-user">erocci_creds:user()</a>, Group::<a href="erocci_creds.md#type-group">erocci_creds:group()</a>, State::term()) -&gt; {{ok, <a href="occi_entity.md#type-t">occi_entity:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="delete-2"></a>

### delete/2 ###

<pre><code>
delete(Location::binary(), State::term()) -&gt; {ok | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Location::binary(), State::term()) -&gt; {{ok, <a href="occi_collection.md#type-t">occi_collection:t()</a> | <a href="occi_entity.md#type-t">occi_entity:t()</a>, <a href="erocci_creds.md#type-user">erocci_creds:user()</a>, <a href="erocci_creds.md#type-group">erocci_creds:group()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Opts::term()) -&gt; {ok, Caps::[<a href="erocci_backend.md#type-capability">erocci_backend:capability()</a>], State::term()} | {error, Reason::term()}
</code></pre>
<br />

<a name="link-4"></a>

### link/4 ###

<pre><code>
link(Location::<a href="occi_uri.md#type-url">occi_uri:url()</a>, Type::source | target, LinkId::<a href="occi_link.md#type-id">occi_link:id()</a>, State::term()) -&gt; {ok | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="mixin-4"></a>

### mixin/4 ###

<pre><code>
mixin(Location::<a href="occi_uri.md#type-url">occi_uri:url()</a>, MixinId::<a href="occi_mixin.md#type-t">occi_mixin:t()</a>, Attributes::<a href="maps.md#type-map">maps:map()</a>, State::term()) -&gt; {{ok, <a href="occi_entity.md#type-t">occi_entity:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="models-1"></a>

### models/1 ###

<pre><code>
models(State::term()) -&gt; {{ok, [<a href="occi_extension.md#type-t">occi_extension:t()</a>]} | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="terminate-1"></a>

### terminate/1 ###

<pre><code>
terminate(State::term()) -&gt; ok
</code></pre>
<br />

<a name="unlink-4"></a>

### unlink/4 ###

<pre><code>
unlink(Location::<a href="occi_uri.md#type-url">occi_uri:url()</a>, Type::source | target, LinkId::<a href="occi_link.md#type-id">occi_link:id()</a>, State::term()) -&gt; {ok | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="unmixin-3"></a>

### unmixin/3 ###

<pre><code>
unmixin(Location::<a href="occi_uri.md#type-url">occi_uri:url()</a>, MixinId::<a href="occi_mixin.md#type-t">occi_mixin:t()</a>, State::term()) -&gt; {{ok, <a href="occi_entity.md#type-t">occi_entity:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

<a name="update-3"></a>

### update/3 ###

<pre><code>
update(Location::<a href="occi_uri.md#type-url">occi_uri:url()</a>, Attributes::<a href="maps.md#type-map">maps:map()</a>, State::term()) -&gt; {{ok, Entity2::<a href="occi_entity.md#type-t">occi_entity:t()</a>, <a href="erocci_node.md#type-serial">erocci_node:serial()</a>} | {error, <a href="erocci_backend.md#type-error">erocci_backend:error()</a>}, NewState::term()}
</code></pre>
<br />

