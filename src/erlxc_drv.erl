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
-module(erlxc_drv).
-include_lib("erlxc/include/erlxc.hrl").

%% API
-export([start/1, start/2, stop/1]).
-export([call/2, encode/2, event/1, event/2]).
-export([getopts/1]).

-spec start(nonempty_string() | binary()) -> port().
-spec start(nonempty_string() | binary(),proplists:proplist()) -> port().
start(Name) ->
    start(Name, []).
start(Name, Options) ->
    Cmd = getopts([{name, Name}] ++ Options),
    open_port({spawn, Cmd}, [{packet, 2}, binary]).

-spec call(port(),binary()) -> 'permanent' | 'transient' | 'temporary' | 'none' | boolean() | iodata() | integer().
call(Port, Data) when is_port(Port), is_binary(Data), byte_size(Data) < 16#ffff ->
    true = erlang:port_command(Port, Data),
    Reply = receive
        {Port, {data, <<?ERLXC_MSG_SYNC, Msg/binary>>}} ->
            binary_to_term(Msg)
    end,
    case Reply of
        badarg -> erlang:error(badarg);
        _ -> Reply
    end.

-spec event(port()) -> {state, binary()}.
-spec event(port(), non_neg_integer() | 'infinity') -> {state, binary()}.
event(Port) when is_port(Port) ->
    event(Port, 0).
event(Port, Timeout) when is_port(Port) ->
    receive
        {Port, {data, <<?ERLXC_MSG_ASYNC, Msg/binary>>}} ->
            binary_to_term(Msg)
    after
        Timeout ->
            false
    end.

-spec encode(integer(),list()) -> <<_:16,_:_*8>>.
encode(Command, Arg) when is_integer(Command), is_list(Arg) ->
    <<Command:2/unsigned-integer-unit:8, (term_to_binary(Arg))/binary>>.

stop(Port) when is_port(Port) ->
    erlang:port_close(Port).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
getopts(Options) when is_list(Options) ->
    Exec = proplists:get_value(exec, Options, "sudo"),
    Progname = proplists:get_value(progname, Options, progname()),

    Expand = lists:map(fun
                    (permanent) -> {type, permanent};
                    (transient) -> {type, transient};
                    (temporary) -> {type, temporary};
                    (nodaemonize) -> {daemonize, false};
                    (nocloseallfds) -> {closeallfds, false};
                    (N) when is_atom(N) -> {N, true};
                    ({_,_} = N) -> N
                end, Options),

    Opt = lists:ukeysort(1, Expand),

    Switches = [ optarg(Arg) || Arg <- Opt ],
    Cmd = [ N || N <- [Exec, Progname|Switches], N /= ""],

    string:join(Cmd, " ").

optarg({name, Arg})             -> switch("n", Arg);
optarg({path, Arg})             -> switch("p", Arg);
optarg({type, permanent})       -> switch("t", "permanent");
optarg({type, transient})       -> switch("t", "transient");
optarg({type, temporary})       -> switch("t", "temporary");
optarg({errlog, Arg})           -> switch("e", Arg);
optarg({daemonize, false})      -> switch("d", "nodaemonize");
optarg({closeallfds, false})    -> switch("d", "nocloseallfds");
optarg({verbose, Arg})          -> switch(string:copies("v", Arg));
optarg(_)                       -> "".

switch(Switch) ->
    lists:concat(["-", Switch]).

switch(Switch, Arg) when is_binary(Arg) ->
    switch(Switch, binary_to_list(Arg));
switch(Switch, Arg) ->
    lists:concat(["-", Switch, " ", Arg]).

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

progname() ->
    filename:join([basedir(erlxc), "erlxc"]).
