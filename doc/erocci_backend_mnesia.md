

# Module erocci_backend_mnesia #
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

__Behaviours:__ [`erocci_backend`](erocci_backend.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


<pre><code>
state() = [<a href="occi_extension.md#type-t">occi_extension:t()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#action-4">action/4</a></td><td></td></tr><tr><td valign="top"><a href="#collection-5">collection/5</a></td><td></td></tr><tr><td valign="top"><a href="#create-4">create/4</a></td><td></td></tr><tr><td valign="top"><a href="#create-5">create/5</a></td><td></td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#link-4">link/4</a></td><td></td></tr><tr><td valign="top"><a href="#mixin-4">mixin/4</a></td><td></td></tr><tr><td valign="top"><a href="#mnesia_disc_copies-1">mnesia_disc_copies/1</a></td><td>Returns nodes on which a mnesia schema must be created.</td></tr><tr><td valign="top"><a href="#models-1">models/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-1">terminate/1</a></td><td></td></tr><tr><td valign="top"><a href="#unmixin-3">unmixin/3</a></td><td></td></tr><tr><td valign="top"><a href="#update-3">update/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="action-4"></a>

### action/4 ###

`action(Location, ActionId, Attributes, S) -> any()`

<a name="collection-5"></a>

### collection/5 ###

`collection(Id, Filter, Start, Number, S) -> any()`

<a name="create-4"></a>

### create/4 ###

`create(Entity, Owner, Group, S) -> any()`

<a name="create-5"></a>

### create/5 ###

`create(Location, Entity, Owner, Group, S) -> any()`

<a name="delete-2"></a>

### delete/2 ###

`delete(Location, S) -> any()`

<a name="get-2"></a>

### get/2 ###

`get(Location, S) -> any()`

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Opts::[]) -&gt; {ok, <a href="erocci_backend.md#type-capability">erocci_backend:capability()</a>, <a href="#type-state">state()</a>} | {error, term()}
</code></pre>
<br />

<a name="link-4"></a>

### link/4 ###

`link(Location, Type, LinkId, S) -> any()`

<a name="mixin-4"></a>

### mixin/4 ###

`mixin(Location, Mixin, Attributes, S) -> any()`

<a name="mnesia_disc_copies-1"></a>

### mnesia_disc_copies/1 ###

`mnesia_disc_copies(X1) -> any()`

Returns nodes on which a mnesia schema must be created

<a name="models-1"></a>

### models/1 ###

`models(S) -> any()`

<a name="terminate-1"></a>

### terminate/1 ###

`terminate(S) -> any()`

<a name="unmixin-3"></a>

### unmixin/3 ###

`unmixin(Location, Mixin, S) -> any()`

<a name="update-3"></a>

### update/3 ###

`update(Location, Attributes, S) -> any()`

