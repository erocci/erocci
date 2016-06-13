

# Module occi_utils #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-mimetype">mimetype()</a> ###


<pre><code>
mimetype() = {Type::binary(), SubType::binary(), Options::list()} | undefined
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#is_absolute-1">is_absolute/1</a></td><td></td></tr><tr><td valign="top"><a href="#mimetype-1">mimetype/1</a></td><td></td></tr><tr><td valign="top"><a href="#mkdir-1">mkdir/1</a></td><td>Recursively creates dir.</td></tr><tr><td valign="top"><a href="#normalize-1">normalize/1</a></td><td>Normalize path: remove duplicate and trailing '/'.</td></tr><tr><td valign="top"><a href="#normalize_mimetype-1">normalize_mimetype/1</a></td><td></td></tr><tr><td valign="top"><a href="#priv_dir-0">priv_dir/0</a></td><td></td></tr><tr><td valign="top"><a href="#resources_dir-0">resources_dir/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="is_absolute-1"></a>

### is_absolute/1 ###

<pre><code>
is_absolute(Path::binary()) -&gt; boolean()
</code></pre>
<br />

<a name="mimetype-1"></a>

### mimetype/1 ###

<pre><code>
mimetype(Path::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; <a href="#type-mimetype">mimetype()</a>
</code></pre>
<br />

<a name="mkdir-1"></a>

### mkdir/1 ###

<pre><code>
mkdir(Dir::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; ok | {error, <a href="file.md#type-posix">file:posix()</a> | badarg}
</code></pre>
<br />

Recursively creates dir

<a name="normalize-1"></a>

### normalize/1 ###

<pre><code>
normalize(Path::binary()) -&gt; binary()
</code></pre>
<br />

Normalize path: remove duplicate and trailing '/'

<a name="normalize_mimetype-1"></a>

### normalize_mimetype/1 ###

<pre><code>
normalize_mimetype(X1::term()) -&gt; <a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>
</code></pre>
<br />

<a name="priv_dir-0"></a>

### priv_dir/0 ###

<pre><code>
priv_dir() -&gt; <a href="file.md#type-filename_all">file:filename_all()</a>
</code></pre>
<br />

<a name="resources_dir-0"></a>

### resources_dir/0 ###

<pre><code>
resources_dir() -&gt; <a href="file.md#type-filename_all">file:filename_all()</a>
</code></pre>
<br />

