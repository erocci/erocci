

# Module occi_rendering #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Provide common functions for rendering/parsing OCCI types.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##

<a name="types"></a>

## Data Types ##




### <a name="type-ast">ast()</a> ###


<pre><code>
ast() = #{<a href="#type-ast_key">ast_key()</a> =&gt; <a href="#type-ast_value">ast_value()</a>}
</code></pre>




### <a name="type-ast_key">ast_key()</a> ###


<pre><code>
ast_key() = categories | attributes | actions | id | links | summary | title | action | source | target | parent | location | depends | applies
</code></pre>




### <a name="type-ast_link_end">ast_link_end()</a> ###


<pre><code>
ast_link_end() = #{location | kind =&gt; binary()}
</code></pre>




### <a name="type-ast_value">ast_value()</a> ###


<pre><code>
ast_value() = binary() | list() | <a href="maps.md#type-map">maps:map()</a> | <a href="#type-ast_link_end">ast_link_end()</a>
</code></pre>




### <a name="type-error">error()</a> ###


<pre><code>
error() = {parse_error, term()} | {unknown_mimetype, term()} | {badkey, atom()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse-3">parse/3</a></td><td>Parse a binary and returns the object using <code>Mod:from_map/1</code>
or the provided function.</td></tr><tr><td valign="top"><a href="#parse_file-2">parse_file/2</a></td><td>Parse file and return an OCCI type
(Tries to) detects mimetype from filename.</td></tr><tr><td valign="top"><a href="#parse_file-3">parse_file/3</a></td><td>Parse file and return an OCCI type
(Tries to) detects mimetype from filename.</td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse-3"></a>

### parse/3 ###

<pre><code>
parse(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Data::iolist(), Fun::<a href="occi_type.md#type-mod">occi_type:mod()</a> | function()) -&gt; <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>
<br />

throws `[errors()](#type-errors)`

Parse a binary and returns the object using `Mod:from_map/1`
or the provided function.

Module must implement from_map/1

Supported mimetypes are:

* {<<"application">>, <<"xml">>, []}

* {<<"application">>, <<"occi+xml">>, []}

* {<<"application">>, <<"json">>, []}

* {<<"application">>, <<"occi+json">>, []}

* {<<"text">>, <<"plain">>, []}

* {<<"text">>, <<"occi">>, []}


<a name="parse_file-2"></a>

### parse_file/2 ###

<pre><code>
parse_file(Path::<a href="file.md#type-filename_all">file:filename_all()</a>, ModOrFun::<a href="occi_type.md#type-mod">occi_type:mod()</a> | function()) -&gt; <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>
<br />

Parse file and return an OCCI type
(Tries to) detects mimetype from filename

<a name="parse_file-3"></a>

### parse_file/3 ###

<pre><code>
parse_file(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Path::<a href="file.md#type-filename_all">file:filename_all()</a>, ModOrFun::<a href="occi_type.md#type-mod">occi_type:mod()</a> | function()) -&gt; <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>
<br />

Parse file and return an OCCI type
(Tries to) detects mimetype from filename

<a name="render-3"></a>

### render/3 ###

<pre><code>
render(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, T::<a href="occi_type.md#type-t">occi_type:t()</a>, Ctx::<a href="occi_uri.md#type-t">occi_uri:t()</a>) -&gt; iolist()
</code></pre>
<br />

