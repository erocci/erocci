

# Module erocci_backends #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td>Get all backends (useful for tagging for instance).</td></tr><tr><td valign="top"><a href="#by_category_id-1">by_category_id/1</a></td><td>Find backends handling category id.</td></tr><tr><td valign="top"><a href="#by_path-1">by_path/1</a></td><td>Find backend attached to mountpoint.</td></tr><tr><td valign="top"><a href="#mount-1">mount/1</a></td><td>Mount a backend.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the supervisor.</td></tr><tr><td valign="top"><a href="#umount-1">umount/1</a></td><td>Stop backend.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-0"></a>

### all/0 ###

<pre><code>
all() -&gt; [<a href="erocci_backend.md#type-t">erocci_backend:t()</a>]
</code></pre>
<br />

Get all backends (useful for tagging for instance)

<a name="by_category_id-1"></a>

### by_category_id/1 ###

<pre><code>
by_category_id(Id::<a href="occi_category.md#type-id">occi_category:id()</a>) -&gt; [<a href="erocci_backend.md#type-t">erocci_backend:t()</a>]
</code></pre>
<br />

Find backends handling category id

<a name="by_path-1"></a>

### by_path/1 ###

<pre><code>
by_path(Path::binary()) -&gt; <a href="erocci_backend.md#type-t">erocci_backend:t()</a>
</code></pre>
<br />

Find backend attached to mountpoint

<a name="mount-1"></a>

### mount/1 ###

<pre><code>
mount(Backend::<a href="erocci_backend.md#type-t">erocci_backend:t()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Mount a backend

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>
<br />

Starts the supervisor

<a name="umount-1"></a>

### umount/1 ###

<pre><code>
umount(Backend::<a href="erocci_backend.md#type-t">erocci_backend:t()</a>) -&gt; ok | {error, not_found}
</code></pre>
<br />

Stop backend

