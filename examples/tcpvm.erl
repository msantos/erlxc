%%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
%%% 
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
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
