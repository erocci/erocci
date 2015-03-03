# About erlang/OTP

erocci is written in erlang and built on top of OTP (the
platform). Running erocci does not require you know the details of
erlang/OTP but some overall knowledge will help.

erlang/OTP should be better seen as an operating system. It provides
process, memory management and a complete abstraction of the
underlying OS mechanisms thereof.

## Release

As many other erlang applications, erocci can be seen as a system,
composed of several applications communicating together through
messages. The set of applications, along with their configuration, is
called a release.

When running `erocci`, you are running a release of erocci.

## Application

An application is a logical entity which can be started and
stopped. It is defined by an application specification containing the
following informations:
* module implementing the `start` and `stop` functions,
* dependant applications,
* environment.

The application environment is a list of key-value pairs which can be
set in different ways, but accessed uniformely. The environment is, of
course, distributed over all nodes where the application
runs. Environment sources are (in priority order, a source overriding
the previous one if necessary):

* <application>.app: provided by developper, basically default values;

* sys.config: provided with a release, means a set of applications
  with specific configuration; there is only one `sys.config` file per
  release, which is divided into sections, one per application;

* erl command line: application enviromnent can be given directly at
  runtime to the VM launcher `erl`. For instance:
  `erl -myapp key1 "value" -myapp key2 42`;

* `application:set_env/3`: programmatically, with the given function.

Last but not least, most of erlang applications runs processes. All
these processes are supervised, either by a specific supervisor, at
least by the default top application supervisor. That means once an
application is started, it runs until it is stopped. erlang/OTP will
deal with crash, if any: beautiful, isn't it ?

## Processes

erlang being a functional language, it allows for drastical
optimization in process and memory mangement: nothing is shared
between processes.

Some figures in the table below (source:
[Erlang Programming, O'Reilly](http://www.amazon.com/dp/0596518188/?tag=stackoverfl08-20),
[Concurrency oriented programming in Erlang, Joe Armstrong](http://www.scribd.com/doc/6505089/Concurrency-Oriented-Programming-in-Erlang-by-Joe-Armstrong),
[Erlang processes vs. Java threads](http://www.lshift.net/blog/2006/09/10/erlang-processes-vs-java-threads/))

|                                    | erlang                               | C#                 | Java               |
|:-----------------------------------|:------------------------------------|:------------------|:------------------|
| Process creation time              | 1µs (up to 2500), 3µs (up to 30 000) | 300µs (up to 2000) | 300µs (up to 2000) |
| Process creation and teardown / sec| 350 000                              | -                  | 11 000             |
| Message delivery                   | 0.8µs                                | 50µs               | 50µs (up to 100 proc.), until 10ms for 1000 proc. |

In a few words, an erlang application can easily run with many
processes.