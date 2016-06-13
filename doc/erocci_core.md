

# Module erocci_core #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (c) 2013-2016 Jean Parpaillon

This file is provided to you under the license described
in the file LICENSE at the root of the project.

You can also download the LICENSE file from the following URL:
https://github.com/erocci/erocci/blob/master/LICENSE

__Behaviours:__ [`application`](application.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start the erocci_core application.</td></tr><tr><td valign="top"><a href="#start_phase-3">start_phase/3</a></td><td>Start phase <code>config</code> start listeners and backends, once
supervisors have been started.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; {ok, [atom()]} | {error, term()}
</code></pre>
<br />

Start the erocci_core application

<a name="start_phase-3"></a>

### start_phase/3 ###

<pre><code>
start_phase(Phase::atom(), Type::atom(), Args::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

Start phase `config` start listeners and backends, once
supervisors have been started

`mnesia` phase: creates disc copies mnesia schema and restart system, if required.

