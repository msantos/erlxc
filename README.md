The goal of erlxc is to be a simple, safe interface to Linux containers
from Erlang.

Status:

* the liblxc binding is mostly stable

* the erlxc module is unstable

Build Instructions
==================

erlxc currently requires using liblxc 1.0.0:
```
git clone https://github.com/lxc/lxc.git
cd lxc
./autogen.sh && ./configure && make && sudo make install
```

You may need to adjust the LXC defaults, for example, to match the
bridge device on your system.
```
vi /usr/local/etc/lxc/default.conf
```

Set these environment variables to indicate the path to the liblxc
install and build:
```
export ERLXC_LDFLAGS="-L /usr/local/lib"
export ERLXC_CFLAGS="-I /usr/local/include"

git clone https://github.com/msantos/erlxc.git
cd erlxc
make
```

The erlxc port will require the appropriate permissions to run. This
can be handled by making the port executable setuid, adding the right
file capabilities or by using `sudo`.
```
$ visudo -f /etc/sudoers.d/99_lxc
username ALL = NOPASSWD: /path/to/erlxc/priv/erlxc
```

erlxc is being tested on Ubuntu 12.04/x86_64 and Ubuntu 13.04/arm
(beaglebone black).

High Level API
==============

The high level API models the container as an Erlang process.

erlxc
-----

    spawn(Name) -> Container
    spawn(Name, Options) -> Container

        Types   Container = container()
                Name = <<>> | binary()
                Options = [
                    {path, iodata()} |
                    {verbose, non_neg_integer()} |
                    {config, [
                        <<>> |
                        {load, file:filename_all()} |
                        {Key, Value} |
                        Key
                    ]} |
                    {cgroup, [{Key, Value}]} |
                    {chroot, [
                        {dir, [DirSpec]} |
                        {copy, [CopySpec]} |
                        {file, [FileSpec]}
                    ]} |
                    {create, [
                        {template, iodata()} |
                        {bdevtype, iodata()} |
                        {bdevspec, iodata()} |
                        {flags, non_neg_integer()} |
                        {argv, [iodata()]}
                    ]} |
                    {start, [
                        {useinit, true | false | 0 | 1} |
                        {argv, list(iodata())}
                    ]} |
                    temporary | transient | temporary
                ]

                Key = iodata()
                Value = iodata()

                DirSpec = file:filename_all()
                    | {file:filename_all(), FileMode}
                FileSpec = {file:filename_all(), FileContent}
                    | {file:filename_all(), FileContent, FileMode}
                CopySpec = {file:filename_all(), file:filename_all()}
                    | {file:filename_all(), file:filename_all(), FileMode}

                FileMode = file:file_info() | integer() | 'undefined'
                FileContent = iodata()

        Spawn a container. If an empty binary is specified as the name,
        erlxc will generate a random name.

    exit(Container, Reason) -> true

        Types   Container = container()
                Reason = normal | kill

        Halt the container. Note: by default, the container is linked
        to the creating process. If the process crashes, so will the
        container.

    send(Container, Data) -> true

        Types   Container = container()
                Data = iodata()

        Send data to the container's console.

    type(Container) -> Type
    type(Container, Type) -> boolean()

        Types   Container = container()
                Type = temporary | transient | permanent

        Get or set the container type. The "type" abuses supervisor
        terminology to describe the behaviour of the container when the
        owning process exits:

            * temporary

              The container is shutdown and the container filesystem
              is deleted.

              Unnamed containers (containers spawned using <<>>) are
              temporary by default.

            * transient

              The container is shutdown when the linked process exits.

              Named containers are transient by default.

            * permanent

              The container continues running if the owning process exits.

    container(Container) -> port()

        Types   Container = container()

        Retrieve the port for the container for use with the liblxc module.

    console(Container) -> port()

        Types   Container = container()

        Retrieve the port for the console.

Examples
--------

* tcpvm

This code listens on a TCP port and spawns an Ubuntu system when a
client connects.

In a shell:
```
$ ./start.sh

% If you're bridge is something other than lxcbr0
> tcpvm:start([{config, [{"lxc.network.link", "br0"}]}]).
```

And in another shell:
```
nc localhost 31337
```

Or if you have `socat` installed, start a session with job control:
```
socat `tty`,raw,echo=0 tcp:localhost:31337
```

```erlang
-module(tcpvm).
-include_lib("erlxc/include/erlxc.hrl").
-export([start/0, start/1]).

start() ->
    start([]).

start(Options) ->
    Port = proplists:get_value(port, Options, 31337),
    {ok, LSock} = gen_tcp:listen(Port, [binary,{active,false},{reuseaddr,true}]),
    accept(LSock, Options).

accept(LSock, Options) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    Pid = spawn(fun() -> create(Socket, Options) end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    accept(LSock, Options).

create(Socket, Options) ->
    Container = erlxc:spawn(<<>>, Options),
    shell(Socket, Container).

shell(Socket, #container{console = Console} = Container) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            erlxc:send(Container, Data),
            shell(Socket, Container);
        {tcp_closed, Socket} ->
            error_logger:error_report([{socket, closed}]),
            ok;
        {tcp_error, Socket, Error} ->
            error_logger:error_report([{socket, Error}]),
            ok;
        {Console, {data, Data}} ->
            ok = gen_tcp:send(Socket, Data),
            shell(Socket, Container)
    end.
```

* imc: run a command inside an "immutable" container

`imc` makes a chroot, does a read-only bind mount of system directories
and mounts writable directories as tmpfs before running the specified
command as a random, unprivileged user.

By default, a distributed erlang node is run. To start the node, compile
the example:

```
make eg
```

Then run a node:

```erlang
./start.sh
% node name will be: imc@192.168.123.45
1> Container = imc:spawn("192.168.123.45").
```

Start up another distributed erlang node and connect:

```
% Assuming 192.168.123.10 is the IP address of your client
erl -name c@192.168.123.10 -setcookie COOKIE
^G
(c@192.168.123.10)1>
User switch command
 --> r 'imc@192.168.123.45'
 --> c
Eshell V5.8.5  (abort with ^G)
(imc@192.168.123.45)1>
```

Any command can be run with these caveats:

* currently stdout/stderr is discarded

* if the command is expecting stdin, unexpected things can happen

* the command must run in the foreground

* the console does not work so the only way to communicate with the
  container is over the network (unless there is shared resource like a
  writeable host directory bind mounted inside the container)

For example, to run a shell inside the container listening on port 2222
with cgroup enforcement disabled:
```erlang
% <<>> : choose a random name
Container = imc:spawn(
        <<>>,
        "192.168.123.45",
        [
            {cgroup, []},
            {cmd, "/usr/bin/mkfifo /tmp/sh; /bin/cat /tmp/sh | /bin/bash -i 2>&1 | /bin/nc -l 2222 > /tmp/sh"}
        ]).
```

Low Level API
=============

The low level API mirrors the liblxc API;

http://qa.linuxcontainers.org/master/current/doc/api/lxccontainer_8h.html

The intent is to create an interface that is more flexible and reliable
than shelling out to the lxc commands.

NULL values are represented by the empty binary (<<>>).

liblxc
------

    list_active_containers(Container, Path) -> List
    list_all_containers(Container, Path) -> List
    list_defined_containers(Container, Path) -> List

        Types   Container = pid()
                Path = iodata()
                List = [binary()]

        List the containers in Path (<<>> uses the default path).

    clear_config(Container) -> true | false

        Types   Container = pid()

        Empty the container LXC configuration.

    clear_config_item(Container, Item) -> true | false

        Types   Container = pid()
                Item = iodata()

        Erase Item from the container's configuration.

    config_file_name(Container) -> Filename

        Types   Container = pid()
                Filename = binary()

        Returns the configuration file for the container.

    create(Container, Template, Bdevtype, Bdevspec, Flags, Argv) -> true | false

        Types   Container = pid()
                Template = iodata()
                Bdevtype = iodata()
                Bdevspec = iodata()
                Flags = integer()
                Argv = [binary()]

        Create a container using the specified LXC template script. Use
        <<>> to specify the default backing store ("dir").

        An example of creating an Ubuntu container:

            {ok, Container} = erlxc_drv:start([{name, <<"testprecise"">>}]),
            true = liblxc:create(
                Container,
                <<"ubuntu">>,
                <<>>,
                <<>>,
                0,
                [<<"-r">>, <<"precise">>]
            ).

    defined(Container) -> true | false

        Types   Container = pid()

        Describes whether the container is defined or not.

    destroy(Container) -> true | false

        Types   Container = pid()

        Removes all resources associated with the container, including
        deleting the container filesystem.

    get_config_item(Container, Item)  -> binary() | 'false'.

        Types   Container = pid()
                Item = iodata()

        Returns the configuration value of the key specified in Item.

    get_config_path(Container) -> binary().

        Types   Container = pid()

        Returns the configuration path for the container.

    get_keys(Container, Key) -> binary().

        Types   Container = pid()
                Key = iodata()

        Retrieves the list of keys from the LXC configuration. A subkey
        can be specified or <<>> to retrieve all keys.

        The keys are returned as a single binary, with individual keys
        separated by '\n'. Use binary:split/3 to convert to a list of keys:

            Keys = liblxc:get_keys(Container, <<>>),
            binary:split(Keys, <<"\n">>, [global,trim]).

    init_pid(Container) -> integer().

        Types   Container = pid()

        Retrieve the PID of the container's init process.

    load_config(Container, Path) -> true | false

        Types   Container = pid()
                Path = iodata()

        Load the container's configuration. An optional path can be
        specified (<<>> to use the default path).

    name(Container) -> binary().

        Types   Container = pid()

        Retrieve the container's name.

    running(Container) -> true | false

        Types   Container = pid()

        Return the status of the container.

    save_config(Container, Path) -> true | false

        Types   Container = pid()

        Save the container configuration to an alternate path.

    set_config_item(Container, Key, Val) -> true | false

        Types   Container = pid()
                Key = Val = iodata()

        Set the configuration key to the specified value.

    set_config_path(Container, Path) -> true | false

        Types   Container = pid()
                Path = iodata()

        Set the configuration path to the specified value.

    shutdown(Container, Timeout) -> true | false

        Types   Container = pid()
                Timeout = non_neg_integer()

        Halt the container by sending a SIGPWR to the container init process.

    start(Container, UseInit, Argv) -> {ok, non_neg_integer()} | {error, file:posix()}

        Types   Container = pid()
                UseInit = 0 | 1
                Argv = [binary()]

        Boot the container. Returns the system PID of the container process.

    state(Container) -> binary()

        Types   Container = pid()

        Retrieves the current state of the container.

    stop(Container) -> true | false

        Types   Container = pid()

        Halts the container.

    wait(Container, State, Timeout) -> true | false

        Types   Container = pid()
                State = iodata()
                Timeout = non_neg_integer()

        Blocks until the container state equals the value specified in
        State. Set Timeout to 0 to return immediately.

Examples
--------

* Create an Ubuntu 12.04 container

```erlang
-module(lxc).
-export([
        define/1, define/2,
        create/1,
        start/1,
        shutdown/1,
        destroy/1
    ]).

define(Name) ->
    define(Name, <<"lxcbr0">>).

define(Name, Bridge) ->
    Container = erlxc_drv:start([{name, Name}]),
    liblxc:set_config_item(Container, <<"lxc.network.type">>, <<"veth">>),
    liblxc:set_config_item(Container, <<"lxc.network.link">>, Bridge),
    liblxc:set_config_item(Container, <<"lxc.network.flags">>, <<"up">>),
    Container.

create(Container) ->
    liblxc:create(Container, <<"ubuntu">>, <<>>, <<>>, 0, [<<"-r">>, <<"precise">>]).

start(Container) ->
    liblxc:start(Container, 0, []).

shutdown(Container) ->
    liblxc:shutdown(Container, 0).

destroy(Container) ->
    liblxc:destroy(Container).
```

Debugging
=========

There are a few options that can be passed to the lxc driver for
debugging:

* nodaemonize : run the container in the foreground

  This option will allow you to see the error messages that are redirected
  to /dev/null after liblxc daemonizes.

* nocloseallfds : valgrind reserves some file descriptors for its
  use. This option ensures liblxc doesn't close them.

To run erlxc under valgrind:

```erlang
Container = erlxc:spawn("foo", [{exec, "sudo  valgrind --leak-check=yes --log-file=/tmp/lxc.log"}, nocloseallfds]).

% Or

Port = erlxc_drv:start("foo", [{exec, "sudo  valgrind --leak-check=yes --log-file=/tmp/lxc.log"}, nocloseallfds]).
```

Alternatives
============

* verx provides an interface to libvirt, which has support for Linux
  containers

https://github.com/msantos/verx

* islet is a high level interface for Linux containers using libvirt

https://github.com/msantos/islet

TODO
====

* generate C/Erlang bindings from typespecs

* generate liblxc documentation

* port to Ubuntu 12.04 (liblxc 0.7.5)
