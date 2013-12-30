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
-module(erlxc_drv).
-include_lib("erlxc/include/erlxc.hrl").

%% API
-export([start/1, start/2, stop/1]).
-export([call/2, encode/2, event/1]).
-export([getopts/1]).

-spec start(nonempty_string() | binary()) -> port().
-spec start(nonempty_string() | binary(),[atom() | tuple()]) -> port().
start(Name) ->
    start(Name, []).
start(Name, Options) ->
    Cmd = getopts([{name, Name}] ++ Options),
    open_port({spawn, Cmd}, [{packet, 2}, binary]).

-spec call(port(),binary()) -> 'permanent' | 'transitory' | 'temporary' | 'none' | boolean() | iodata() | integer().
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

-spec event(port()) -> {start, boolean()}.
event(Port) when is_port(Port) ->
    receive
        {Port, {data, <<?ERLXC_MSG_ASYNC, Msg/binary>>}} ->
            binary_to_term(Msg)
    after
        0 ->
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

    Expand = [ begin
                case N of
                    permanent -> {type, permanent};
                    transitory -> {type, transitory};
                    temporary -> {type, temporary};
                    _ when is_atom(N) -> {N, true};
                    _ when is_tuple(N) -> N
                end
        end || N <- Options ],

    Opt = lists:ukeysort(1, Expand),

    Switches = [ optarg(Arg) || Arg <- Opt ],
    Cmd = [ N || N <- [Exec, Progname|Switches], N /= ""],

    string:join(Cmd, " ").

optarg({name, Arg})         -> switch("n", Arg);
optarg({path, Arg})         -> switch("p", Arg);
optarg({type, permanent})   -> switch("t", "permanent");
optarg({type, transitory})  -> switch("t", "transitory");
optarg({type, temporary})   -> switch("t", "temporary");
optarg({errlog, Arg})       -> switch("e", Arg);
optarg({verbose, Arg})      -> string:copies("-v ", Arg);
optarg(_)                   -> "".

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
                "priv",
                Module
            ]);
        Dir ->
            Dir
        end.

progname() ->
    filename:join([basedir(erlxc), "erlxc"]).
