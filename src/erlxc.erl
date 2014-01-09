%%% Copyright (c) 2013-2014, Michael Santos <michael.santos@gmail.com>
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
-include_lib("kernel/include/file.hrl").

-export([
        spawn/0, spawn/1, spawn/2,

        type/1, type/2,
        temporary/1, transitory/1, permanent/1,

        send/2,
        exit/2,

        container/1,
        console/1
    ]).
-export([
        new/0, new/1, new/2,
        attach/1, attach/2,
        chroot/2,
        config/2,
        create/2,
        start/2
    ]).

-spec spawn() -> #container{}.
-spec spawn(string() | binary()) -> #container{}.
-spec spawn(string() | binary(),proplists:proplist()) -> #container{}.
spawn() ->
    erlxc:spawn(<<>>, []).
spawn(Name) ->
    erlxc:spawn(Name, []).
spawn(Name, Options) ->
    Port = new(Name, Options),
    boot(#container{port = Port}, Options).

-spec send(#container{},iodata()) -> 'true'.
send(#container{console = Console}, Data) ->
    erlxc_console:send(Console, Data).

-spec exit(#container{},'kill' | 'normal') -> boolean().
exit(#container{port = Port}, normal) ->
    liblxc:shutdown(Port, 0);

exit(#container{port = Port}, kill) ->
    liblxc:stop(Port).

type(#container{port = Port}) ->
    liblxc:type(Port).

type(Container, Type) when
    Type =:= temporary;
    Type =:= transitory;
    Type =:= permanent ->
    ?MODULE:Type(Container).

temporary(#container{port = Port}) ->
    liblxc:temporary(Port).

transitory(#container{port = Port}) ->
    liblxc:transitory(Port).

permanent(#container{port = Port}) ->
    liblxc:permanent(Port).

container(#container{port = Port}) -> Port.
console(#container{console = Port}) -> Port.

%%--------------------------------------------------------------------
%%% Container configuration
%%--------------------------------------------------------------------
-spec new() -> port().
-spec new(string() | binary()) -> port().
-spec new(string() | binary(),proplists:proplist()) -> port().
new() ->
    new(<<>>, []).
new(Name) ->
    new(Name, []).
new(<<>>, Options) ->
    new(name(<<"erlxc">>), Options ++ [temporary]);
new(Name, Options) ->
    erlxc_drv:start(Name, Options ++ [transitory]).

-spec attach(#container{}) -> #container{}.
-spec attach(#container{}, proplists:proplist()) -> #container{}.
attach(Container) ->
    attach(Container, []).
attach(#container{port = Port, console = undefined} = Container, Options) ->
    Name = liblxc:name(Port),
    Console = erlxc_console:start(Name, Options),
    Container#container{console = Console};
attach(#container{console = Console0} = Container, Options) ->
    catch erlxc_console:stop(Console0),
    attach(Container#container{console = undefined}, Options).

-spec config(#container{}, proplists:proplist()) -> 'ok'.
config(#container{port = Port}, Options) ->
    Path = proplists:get_value(config_path, Options, liblxc:get_config_path(Port)),
    true = liblxc:set_config_path(Port, Path),

    Config = proplists:get_value(config, Options, []),

    verbose(1, {config_path, [
                liblxc:get_config_path(Port),
                liblxc:config_file_name(Port)
            ]}, Options),

    lists:foreach(fun
            (<<>>) ->
                verbose(1, {clear_config, [Port]}, Options),
                true = liblxc:clear_config(Port);
            ({Key, Value}) ->
                verbose(1, {set_config_item, [Port, Key, Value]}, Options),
                true = liblxc:set_config_item(Port, Key, Value);
            (Key) ->
                verbose(1, {clear_config_item, [Port, Key]}, Options),
                true = liblxc:clear_config_item(Port, Key)
        end, Config),

    true = liblxc:save_config(Port, liblxc:config_file_name(Port)),
    ok.

-spec chroot(#container{}, proplists:proplist()) -> 'ok'.
chroot(#container{port = Port}, Options) ->
    ConfigPath = proplists:get_value(config_path, Options, liblxc:get_config_path(Port)),
    Chroot = proplists:get_value(chroot, Options, []),

    Dir = proplists:get_value(dir, Chroot, []),
    Copy = proplists:get_value(copy, Chroot, []),
    File = proplists:get_value(file, Chroot, []),

    Name = liblxc:name(Port),
    Path = <<(maybe_binary(ConfigPath))/binary, "/", Name/binary>>,

    make(dir, Path, Dir, Options),
    make(copy, Path, Copy, Options),
    make(file, Path, File, Options).

-spec create(#container{}, proplists:proplist()) -> 'ok'.
create(#container{port = Port}, Options) ->
    Create = proplists:get_value(create, Options, []),

    Template = proplists:get_value(template, Create, <<"ubuntu">>),
    Bdevtype = proplists:get_value(bdevtype, Create, <<>>),
    Bdevspec = proplists:get_value(bdevspec, Create, <<>>),
    Flags = proplists:get_value(flags, Create, 0),
    Argv = proplists:get_value(argv, Create, []),

    verbose(1, {create, [Port, Template, Bdevtype, Bdevspec, Flags, Argv]}, Options),
    true = liblxc:create(Port, Template, Bdevtype, Bdevspec, Flags, Argv),
    ok.

-spec start(#container{}, proplists:proplist()) -> 'ok'.
start(#container{port = Port}, Options) ->
    Start = proplists:get_value(start, Options, []),
    UseInit = proplists:get_value(useinit, Start, false),
    Argv = proplists:get_value(argv, Start, []),

    verbose(1, {start, [Port, UseInit, Argv]}, Options),
    true = liblxc:start(Port, bool(UseInit), Argv),
    ok.

%%--------------------------------------------------------------------
%%% Container state
%%--------------------------------------------------------------------

% "STOPPED", "STARTING", "RUNNING", "STOPPING",
% "ABORTING", "FREEZING", "FROZEN", "THAWED",
boot(#container{port = Port} = Container, Options) ->
    true = liblxc:async_state_notify(Port, 5),
    state(Container, Options).

state(#container{port = Port} = Container, Options) ->
    Path = proplists:get_value(config_path, Options, liblxc:get_config_path(Port)),
    true = liblxc:set_config_path(Port, Path),
    state(Container, liblxc:defined(Port), Options).
state(#container{port = Port} = Container, false, Options) ->
    case erlxc_drv:event(Port, infinity) of
        {state, <<"STOPPED">>} ->
            chroot(Container, Options),
            create(Container, Options),
            config(Container, Options),
            start(Container, Options),
            state(Container, Options);
        {state, State} ->
            erlang:error({error, State})
    end;
state(#container{port = Port} = Container, true, Options) ->
    case erlxc_drv:event(Port, infinity) of
        {state, <<"RUNNING">>} ->
            true = liblxc:async_state_close(Port),
            attach(Container);
        {state, <<"STOPPED">>} ->
            config(Container, Options),
            start(Container, Options),
            state(Container, Options);
        {state, <<"FROZEN">>} ->
            true = liblxc:unfreeze(Port),
            state(Container, true, Options);
        {state, State} when State =:= <<"STARTING">>; State =:= <<"STOPPING">>;
                State =:= <<"FREEZING">>; State =:= <<"THAWED">> ->
            state(Container, Options);
        {state, State} ->
            erlang:error({unsupported, State, Options})
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
bool(true) -> 1;
bool(false) -> 0;
bool(1) -> 1;
bool(0) -> 0.

i2b(N) ->
    list_to_binary(integer_to_list(N)).

name(Name) ->
    % XXX possible to re-use container names
    N = binary:decode_unsigned(crypto:rand_bytes(4)),
    <<Name/binary, (i2b(N))/binary>>.

verbose(Level, Msg, Opt) ->
    Verbose = proplists:get_value(verbose, Opt, 0),
    if
        Verbose >= Level ->
            error_logger:info_report([Msg, {options, Opt}]);
        true ->
            ok
    end.

maybe_binary(N) when is_list(N) -> list_to_binary(N);
maybe_binary(N) when is_binary(N) -> N.

make(dir, Path, List, Options) ->
    lists:foreach(fun
            ({Dir, Info}) ->
                verbose(1, {dir, [Dir, Info]}, Options),
                ok = dir(Path, Dir, Info);
            (Dir) ->
                verbose(1, {dir, [Dir]}, Options),
                ok = dir(Path, Dir)
        end, List);
make(copy, Path, List, Options) ->
    lists:foreach(fun
            ({Source, Destination}) ->
                verbose(1, {copy, [Source, Destination]}, Options),
                ok = copy(Path, Source, Destination);
            ({Source, Destination, Info}) ->
                verbose(1, {copy, [Source, Destination, Info]}, Options),
                ok = copy(Path, Source, Destination, Info)
        end, List);
make(file, Path, List, Options) ->
    lists:foreach(fun
            ({File, Content, Info}) ->
                verbose(1, {file, [File, Content, Info]}, Options),
                ok = file(Path, File, Content, Info);
            ({File, Content}) ->
                verbose(1, {file, [File, Content]}, Options),
                ok = file(Path, File, Content);
            (File) when is_binary(File); is_list(File) ->
                verbose(1, {file, [File]}, Options),
                ok = file(Path, File, <<>>)
        end, List).

dir(Path, Dir) ->
    dir(Path, Dir, undefined).
dir(Path, Dir, Info) ->
    dir_1(Path, maybe_binary(Dir), Info).
dir_1(_Path, <<"/", _/binary>> = Dir0, Info) ->
    Dir = <<Dir0/binary, "/">>,
    case filelib:ensure_dir(Dir) of
        ok ->
            write_file_info(Dir, Info);
        Error ->
            Error
    end;
dir_1(Path, Dir0, Info) ->
    Dir = <<Path/binary, "/", Dir0/binary, "/">>,
    case filelib:ensure_dir(Dir) of
        ok ->
            write_file_info(Dir, Info);
        Error ->
            Error
    end.

copy(Path, Source, Destination) ->
    copy(Path, Source, Destination, undefined).
copy(Path, Source, Destination, Info) ->
    copy_1(Path, maybe_binary(Source), maybe_binary(Destination), Info).
copy_1(_Path, Source, <<"/",_/binary>> = Destination, Info) ->
    case file:copy(Source, Destination) of
        {ok, _} ->
            write_file_info(Destination, Info);
        Error ->
            Error
    end;
copy_1(Path, Source, Destination0, Info) ->
    Destination = <<Path/binary, "/", Destination0/binary>>,
    case file:copy(Source, Destination) of
        {ok, _} ->
            write_file_info(Destination, Info);
        Error ->
            Error
    end.

file(Path, File, Content) ->
    file(Path, File, Content, undefined).
file(Path, File, Content, Info) ->
    file_1(Path, maybe_binary(File), Content, Info).
file_1(_Path, <<"/", _/binary>> = File, Content, Info) ->
    case file:write_file(File, Content) of
        ok ->
            write_file_info(File, Info);
        Error ->
            Error
    end;
file_1(Path, File0, Content, Info) ->
    File = <<Path/binary, "/", File0/binary>>,
    case file:write_file(File, Content) of
        ok ->
            write_file_info(File, Info);
        Error ->
            Error
    end.

write_file_info(_File, undefined) ->
    ok;
write_file_info(File, #file_info{} = Info) ->
    file:write_file_info(File, Info);
write_file_info(File, Mode) when is_integer(Mode) ->
    file:write_file_info(File, #file_info{mode = Mode}).
