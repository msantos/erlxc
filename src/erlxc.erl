%%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
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
-module(erlxc).
-include_lib("erlxc/include/erlxc.hrl").

-export([
        spawn/0, spawn/1, spawn/2,
        send/2,
        exit/2
    ]).

% Can be overriden by caller
-define(SPAWN_DEFAULT, [destroy]).

-type container() :: #container{port::port(),console::port()}.

-spec spawn() -> container().
-spec spawn(string() | binary()) -> container().
-spec spawn(string() | binary(),list()) -> container().
spawn() ->
    erlxc:spawn(<<>>, []).

spawn(Name) ->
    erlxc:spawn(Name, []).

spawn(<<>>, Options) ->
    % XXX possible to re-use container names
    N = binary:decode_unsigned(crypto:rand_bytes(4)),
    Name = <<"erlxc", (i2b(N))/binary>>,
    erlxc:spawn(Name, Options);
spawn(Name, Options) ->
    Container = erlxc_drv:start(Name, ?SPAWN_DEFAULT ++ Options),
    defined(Container, Options).

-spec send(container(),iodata()) -> 'true'.
send(#container{console = Console}, Data) ->
    erlxc_console:send(Console, Data).

-spec exit(container(),'kill' | 'normal') -> boolean().
exit(#container{port = Port}, normal) ->
    liblxc:shutdown(Port, 0);

exit(#container{port = Port}, kill) ->
    liblxc:stop(Port).

defined(Container, Options) ->
    case liblxc:defined(Container) of
        true ->
            running(Container, Options, fun config/2);
        false ->
            create(Container, Options)
    end.

running(Container, Options, Fun) ->
    case liblxc:running(Container) of
        true ->
            Console = erlxc_console:start(liblxc:name(Container)),
            #container{port = Container, console = Console};
        false ->
            Fun(Container, Options)
    end.

create(Container, Options) ->
    Create = proplists:get_value(create, Options, []),
    Template = proplists:get_value(template, Create, <<"ubuntu">>),
    Bdevtype = proplists:get_value(bdevtype, Create, <<>>),
    Bdevspec = proplists:get_value(bdevspec, Create, <<>>),
    Flags = proplists:get_value(flags, Create, 0),
    Argv = proplists:get_value(argv, Create, []),

    true = liblxc:create(Container, Template, Bdevtype, Bdevspec, Flags, Argv),
    config(Container, Options).

config(Container, Options) ->
    Daemonize = proplists:get_value(daemonize, Options, false),

    Config = proplists:get_value(config, Options, []),

    [ begin
        case Item of
            {Key, Value} ->
                true = liblxc:set_config_item(Container, Key, Value);
            Key ->
                true = liblxc:clear_config_item(Container, Key)
        end
      end || Item <- Config ],

    true = liblxc:daemonize(Container, bool(Daemonize)),

    start(Container, Options).

start(Container, Options) ->
    UseInit = proplists:get_value(useinit, Options, false),
    Path = proplists:get_value(path, Options, []),

    true = liblxc:start(Container, bool(UseInit), Path),
    running(Container, Options, fun(_,_) -> erlang:exit(badarg) end).

bool(true) -> 1;
bool(false) -> 0;
bool(1) -> 1;
bool(0) -> 0.

i2b(N) ->
    list_to_binary(integer_to_list(N)).
