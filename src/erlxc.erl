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

-export([
        spawn/1, spawn/2,
        exit/2
    ]).

spawn(Name) ->
    erlxc:spawn(Name, []).

spawn(Name, Options) ->
    case erlxc_drv:start(Name, Options) of
        {ok, Container} ->
            defined(Container, Options);
        Error ->
            Error
    end.

exit(Container, normal) ->
    liblxc:shutdown(Container, 0);

exit(Container, kill) ->
    liblxc:stop(Container).

defined(Container, Options) ->
    case liblxc:defined(Container) of
        true ->
            running(Container, Options);
        false ->
            create(Container, Options)
    end.

running(Container, Options) ->
    case liblxc:running(Container) of
        true ->
            {ok, Container};
        false ->
            config(Container, Options)
    end.

create(Container, Options) ->
    Create = proplists:get_value(create, Options, []),
    Template = proplists:get_value(template, Create, <<"ubuntu">>),
    Bdevtype = proplists:get_value(bdevtype, Create, <<>>),
    Bdevspec = proplists:get_value(bdevspec, Create, <<>>),
    Flags = proplists:get_value(flags, Create, <<>>),
    Argv = proplists:get_value(argv, Create, []),

    case liblxc:create(Container, Template, Bdevtype, Bdevspec, Flags, Argv) of
        true ->
            config(Container, Options);
        false ->
            {error, einval}
    end.

config(Container, Options) ->
    Daemonize = proplists:get_value(daemonize, Options, false),

    Config = proplists:get_value(config, Options, []),

    [ true = liblxc:set_config_item(Container, Key, Value) ||
        {Key, Value} <- Config ],

    true = liblxc:daemonize(Container, bool(Daemonize)),

    start(Container, Options).

start(Container, Options) ->
    UseInit = proplists:get_value(useinit, Options, false),
    Path = proplists:get_value(path, Options, []),

    case liblxc:start(Container, bool(UseInit), Path) of
        {ok, _} ->
            case liblxc:running(Container) of
                true ->
                    {ok, Container};
                false ->
                    erlang:exit(badarg)
            end;
        Error ->
            Error
    end.

bool(true) -> 1;
bool(false) -> 0;
bool(1) -> 1;
bool(0) -> 0.
