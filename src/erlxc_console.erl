%%% Copyright (c) 2013-2014, Michael Santos <michael.santos@gmail.com>
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
-spec start(nonempty_string() | binary(),proplists:proplist()) -> port().
start(Name) ->
    start(Name, []).
start(Name, Options) when is_list(Options) ->
    [Cmd|Argv] = getopts([{name, Name}] ++ Options),
    open_port({spawn_executable, Cmd}, [{args,Argv},stream,binary]).

send(Container, Data) when is_port(Container) ->
    erlang:port_command(Container, Data).

stop(Port) ->
    erlang:port_close(Port).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec getopts(proplists:proplist()) -> list(string() | [string()]).
getopts(Options) when is_list(Options) ->
    Exec = proplists:get_value(exec, Options, "sudo"),
    Progname = proplists:get_value(progname, Options, progname()),

    Switches = [ optarg(Opt) || Opt <- proplists:compact(Options) ],
    [Cmd0|Argv] = [ N || N <- [Exec, Progname|Switches], N /= ""],

    Cmd = string:join([find_executable(Cmd0)|Argv], " "),

    [find_executable("script"), "-q", "-c", Cmd, "/dev/null"].

optarg({name, Arg})         -> switch("n", Arg);
optarg({path, Arg})         -> switch("P", Arg);
optarg({escape, Arg})       -> switch("e", Arg);
optarg({tty, Arg})          -> switch("t", Arg);
optarg(_)                   -> "".

switch(Switch, Arg) when is_binary(Arg) ->
    switch(Switch, binary_to_list(Arg));
switch(Switch, Arg) ->
    lists:concat(["-", Switch, " ", Arg]).

find_executable(Exe) ->
    case os:find_executable(Exe) of
        false ->
            erlang:error({executable_not_found, Exe});
        N ->
            N
    end.

progname() ->
    find_executable("lxc-console").
