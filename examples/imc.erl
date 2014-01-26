%%% Copyright (c) 2013-2014, Michael Santos <michael.santos@gmail.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR

% imc: a read-only container
%
% Build a chroot and run an unprivileged command inside the container.
% Read-only directories are bind mounted from the host. Writable
% directories are mounted as tmpfs.
%
% The command is run under a random UID/GID. stdout/stderr is not reported
% back to erlang, so the only way to communicate with the container is via
% the network.
%
% By default, a distributed erlang node is run. To start the node, compile
% the example:
%
%     make eg
%
% Then run a node:
%
%     ./start.sh
%     % node name will be: imc@192.168.213.45
%     1> Container = imc:spawn("192.168.123.45").
%
% Start up another distributed erlang node and connect:
%
%     % Assuming 192.168.213.10 is the IP address of your client
%     erl -name c@192.168.123.10 -setcookie COOKIE
%     ^G
%     (c@192.168.123.10)1>
%     User switch command
%      --> r 'imc@192.168.123.45'
%      --> c
%     Eshell V5.8.5  (abort with ^G)
%     (imc@192.168.123.45)1>
%
-module(imc).
-export([
        spawn/1, spawn/3,
        opt/1,
        config/0,
        chroot/0,
        init_path/0
    ]).

spawn(IPaddr) ->
    ?MODULE:spawn(<<>>, IPaddr, []).
spawn(Name, IPaddr, Options) ->
    Opt = opt([{ipaddr, IPaddr}|Options]),
    erlxc:spawn(Name, Opt).

opt(Opt) ->
    Path = proplists:get_value(path, Opt, "/tmp/imc"),
    IPaddr = proplists:get_value(ipaddr, Opt, "127.0.0.1"),
    Bridge = proplists:get_value(bridge, Opt, "br0"),
    Cgroup = proplists:get_value(cgroup, Opt, cgroup()),

    UID = maybe_list(proplists:get_value(uid, Opt, 16#F0000000 + erlang:phash2({node(),self()}, 16#ffff))),
    GID = maybe_list(proplists:get_value(gid, Opt, UID)),
    Cookie = maybe_list(proplists:get_value(cookie, Opt, "COOKIE")),
    Cmd = maybe_list(proplists:get_value(
            cmd,
            Opt,
            "erl -noinput -setcookie " ++ Cookie ++ " -name imc@" ++ IPaddr
        )),

    Argv = [
        "/sbin/init", UID, GID,
        "/bin/sh", "-c",
            "PATH=/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin "
            "HOME=/home/imc " ++ Cmd
        ],

    Config = config() ++ [
        {"lxc.network.type", "veth"},
        {"lxc.network.flags", "up"},
        {"lxc.network.link", Bridge},
        {"lxc.network.ipv4", IPaddr},
        {"lxc.mount.entry", "tmpfs home/imc tmpfs uid=" ++ UID ++ ",gid=" ++ GID ++ ",noatime,mode=1777,nosuid,size=128M 0 0"}
    ],

    [temporary,
     {verbose, 1},
     {path, Path},
     {config, Config},
     {chroot, chroot()},
     {cgroup, Cgroup},
     {start, [{argv, Argv}]}].

config() ->
    [
        {"lxc.cgroup.devices.deny", "a"},
        {"lxc.cgroup.devices.allow", "c 1:3 rwm"},
        {"lxc.cgroup.devices.allow", "c 1:5 rwm"},
        {"lxc.cgroup.devices.allow", "c 1:8 rwm"},
        {"lxc.cgroup.devices.allow", "c 1:9 rwm"},
        {"lxc.cgroup.devices.allow", "c 5:2 rwm"},
        {"lxc.cgroup.devices.allow", "c 136:* rwm"},
        {"lxc.cgroup.devices.allow", "c 1:7 rwm"},
        {"lxc.cgroup.cpuset.cpus", "0"},
        {"lxc.cgroup.cpu.shares", "256"},
        {"lxc.mount.entry", "/lib lib none ro,bind,nosuid 0 0"},
        {"lxc.mount.entry", "/bin bin none ro,bind,nosuid 0 0"},
        {"lxc.mount.entry", "/usr usr none ro,bind,nosuid 0 0"},
        {"lxc.mount.entry", "/sbin sbin none ro,bind,nosuid 0 0"},
        {"lxc.mount.entry", init_path() ++ " sbin/init none ro,bind 0 0"},
        {"lxc.mount.entry", "tmpfs tmp tmpfs noatime,mode=1777,nosuid,size=128M 0 0"},
        {"lxc.mount.entry", "/dev dev none ro,bind,nosuid 0 0"},
        {"lxc.pts", "1024"},
        {"lxc.mount.entry", "devpts dev/pts devpts rw,noexec,nosuid,gid=5,mode=0620 0 0"},
        {"lxc.mount.entry", "proc proc proc nodev,noexec,nosuid 0 0"} |
        exists([
                "/lib64",
                "/etc/alternatives"
            ])
    ].

cgroup() ->
    [{"blkio.weight", "500"},
     {"memory.soft_limit_in_bytes", "268435456"},
     {"memory.limit_in_bytes", "53687091"}].

chroot() ->
    [{dir, [
        "run",
        "run/shm",
        "home",
        "home/imc",
        "sbin",
        "selinux",
        "sys",
        "tmp",
        "lib",
        "dev",
        "dev/pts",
        "etc",
        "etc/alternatives",
        "root",
        "boot",
        "var",
        "var/run",
        "var/log",
        "usr",
        "bin",
        "lib64",
        "proc"
    ]}].

exists(Dirs) ->
    exists(Dirs, []).
exists([], Acc) ->
    lists:reverse(Acc);
exists([Dir|Dirs], Acc) ->
    case file:read_file_info(Dir) of
        {ok, _} ->
            "/" ++ RelPath = Dir,
            exists(Dirs, [{"lxc.mount.entry", Dir ++ " " ++ RelPath ++ " none ro,bind,nosuid 0 0"}|Acc]);
        {error, _} ->
            exists(Dirs, Acc)
    end.

maybe_list(N) when is_list(N) -> N;
maybe_list(N) when is_integer(N) -> integer_to_list(N).

basedir(Module) ->
    case code:priv_dir(Module) of
        {error, bad_name} ->
            filename:join([
                filename:dirname(code:which(Module)),
                "..",
                "priv"
            ]);
        Dir ->
            Dir
    end.

init_path() ->
    filename:join([basedir(erlxc), "erlxc_exec"]).
