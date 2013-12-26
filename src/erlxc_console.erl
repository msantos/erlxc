%%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(erlxc_console).

%% API
-export([start/1, start/2, stop/1]).
-export([send/2]).
-export([getopts/1]).

-spec start(nonempty_string() | binary()) -> port().
-spec start(nonempty_string() | binary(),[atom() | tuple()]) -> port().
start(Name) ->
    start(Name, []).
start(Name, Options) when is_list(Options) ->
    Cmd = getopts([{name, Name}] ++ Options),
    Port = open_port({spawn, Cmd}, [stream,binary]),
    receive
        {Port, {data,_}} ->
            Port
    end.

send(Container, Data) when is_port(Container) ->
    erlang:port_command(Container, Data).

stop(Port) ->
    erlang:port_close(Port).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
getopts(Options) when is_list(Options) ->
    Exec = proplists:get_value(exec, Options, "sudo"),
    Progname = proplists:get_value(progname, Options, progname()),

    Switches = [ optarg(Opt) || Opt <- proplists:compact(Options) ],
    Cmd0 = [ N || N <- [Exec, Progname|Switches], N /= ""],
    Cmd = quote(string:join(Cmd0, " ")),

    string:join(["script", "-q", "-c", Cmd, "/dev/null"], " ").

optarg({name, Arg})         -> switch("n", Arg);
optarg({path, Arg})         -> switch("P", Arg);
optarg({escape, Arg})       -> switch("e", Arg);
optarg({tty, Arg})          -> switch("t", Arg);
optarg(_)                   -> "".

switch(Switch, Arg) when is_binary(Arg) ->
    switch(Switch, binary_to_list(Arg));
switch(Switch, Arg) ->
    lists:concat(["-", Switch, " ", Arg]).

quote("") ->
    "";
quote(Str) ->
    "\"" ++ Str ++ "\"".

progname() ->
    "lxc-console".
