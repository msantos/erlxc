The goal of erlxc is to be a simple, safe interface to Linux containers
from Erlang.

Status: under development, unstable

# Low Level API

The low level API mirrors the liblxc API;

http://qa.linuxcontainers.org/master/current/doc/api/lxccontainer_8h.html

The intent is to create an interface that is more flexible and reliable
than shelling out to the lxc commands.

NULL values are represented by the empty binary (<<>>).

## liblxc

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

    daemonize(Container, Daemonize) -> true | false

        Types   Container = pid()
                Daemonize = 0 | 1

        By default, the container is attached to the erlxc port. When
        the Erlang side crashes or exits, the containers are shut down.
        Setting the value of Daemonize to 1 results in the container
        from detaching from the Erlang process.

    defined(Container) -> true | false

        Types   Container = pid()

        Describes whether the container is defined or not.

    destroy(Container) -> true | false

        Types   Container = pid()

        Removes all resources associated with the container, including
        deleting the container filesystem.

    get_config_item(Container, Item)  -> binary() | 'none'.

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

    start(Container, UseInit, Path) -> {ok, non_neg_integer()} | {error, file:posix()}

        Types   Container = pid()
                UseInit = 0 | 1
                Path = iodata()

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

# High Level API

The high level API models the container as an Erlang process:

    {ok, Pid} = erlxc:spawn(Ref),
    ok = erlxc:send(Pid, Data), % send to the container STDIN
    {ok, Data} = erlxc:recv(Pid). % read from STDOUT

# Alternatives

* verx provides an interface to libvirt, which has support for Linux
  containers

https://github.com/msantos/verx

* islet is a high level interface for Linux containers using libvirt

https://github.com/msantos/islet

# Examples

## Create an Ubuntu 12.04 container

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
        {ok, Container} = erlxc_drv:start([{name, Name}]),
        liblxc:set_config_item(Container, <<"lxc.network.type">>, <<"veth">>),
        liblxc:set_config_item(Container, <<"lxc.network.link">>, Bridge),
        liblxc:set_config_item(Container, <<"lxc.network.flags">>, <<"up">>),
        Container.

    create(Container) ->
        liblxc:create(Container, <<"ubuntu">>, <<>>, <<>>, 0, [<<"-r">>, <<"precise">>]).

    start(Container) ->
        liblxc:start(Container).

    shutdown(Container) ->
        liblxc:shutdown(Container, 0).

    destroy(Container) ->
        liblxc:destroy(Container).
