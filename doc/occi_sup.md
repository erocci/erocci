

# Module occi_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Supervisor for the occi core application.

Copyright (c) 2013 Jean Parpaillon.

This file is provided to you under the Apache License,
Version 2.0 (the "License"); you may not use this file
except in compliance with the License.  You may obtain
a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td>supervisor callback.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>API for starting the supervisor.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(X1::[]) -&gt; SupervisorTree
</code></pre>
<br />

supervisor callback.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; ServerRet
</code></pre>
<br />

API for starting the supervisor.

