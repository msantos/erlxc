The goal of erlxc is to be a simple, safe interface to Linux containers
from Erlang.

## Low Level API

The low level API mirrors the liblxc API;

http://qa.linuxcontainers.org/master/current/doc/api/lxccontainer_8h.html

The intent is to create an interface that is more flexible and reliable
than shelling out to the lxc commands.

## High Level API

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
