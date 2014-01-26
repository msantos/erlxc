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
        temporary/1, transient/1, permanent/1,

        send/2,
        exit/2,

        container/1,
        console/1
    ]).
-export([
        new/0, new/1, new/2,
        connect/1, connect/2,
        chroot/2,
        config/2,
        create/2,
        start/2
    ]).
-export([dir/2, copy/2, file/2]).

-export_type([container/0,options/0]).

-type container() :: #container{}.

-type config_options() :: {'config', [
    <<>> |
    {'load', file:filename_all()} |
    {iodata(), iodata()} |
    iodata()
]}.

-type cgroup_options() :: {'cgroup', [
    {iodata(), iodata()}
]}.

-type chroot_options() :: {'chroot', [
    {'dir', list(file:filename_all())} |
    {'copy', list(file:filename_all())} |
    {'file', list(file:filename_all())}
]}.

-type create_options() :: {'create', [
    {'template', iodata()} |
    {'bdevtype', iodata()} |
    {'bdevspec', iodata()} |
    {'flags', non_neg_integer()} |
    {'argv', list(iodata())}
]}.

-type start_options() :: {'start', [
    {'useinit', 'true' | 'false' | 0 | 1} |
    {'argv', list(iodata())}
]}.

-type options() :: [
    {'path', iodata()} |
    {'timeout', non_neg_integer() | 'infinity'} |
    {'verbose', non_neg_integer()} |
    config_options() |
    cgroup_options() |
    chroot_options() |
    create_options() |
    start_options() |
    erlxc_drv:type()
].

-spec spawn() -> container().
-spec spawn(string() | binary()) -> container().
-spec spawn(string() | binary(),options()) -> container().
spawn() ->
    erlxc:spawn(<<>>, []).
spawn(Name) ->
    erlxc:spawn(Name, []).
spawn(Name, Options) ->
    Container = new(Name, Options),
    boot(Container, Options).

-spec send(container(),iodata()) -> 'true'.
send(#container{console = Console}, Data) ->
    erlxc_console:send(Console, Data).

-spec exit(container(),'kill' | 'normal') -> boolean().
exit(#container{port = Port}, normal) ->
    liblxc:shutdown(Port, 0);

exit(#container{port = Port}, kill) ->
    liblxc:stop(Port).

type(#container{port = Port}) ->
    liblxc:type(Port).

-spec type(container(), erlxc_drv:type()) -> 'true'.
type(Container, Type) when
    Type =:= temporary;
    Type =:= transient;
    Type =:= permanent ->
    ?MODULE:Type(Container).

temporary(#container{port = Port}) ->
    liblxc:temporary(Port).

transient(#container{port = Port}) ->
    liblxc:transient(Port).

permanent(#container{port = Port}) ->
    liblxc:permanent(Port).

container(#container{port = Port}) -> Port.
console(#container{console = Port}) -> Port.

%%--------------------------------------------------------------------
%%% Container configuration
%%--------------------------------------------------------------------
-spec new() -> container().
-spec new(string() | binary()) -> container().
-spec new(string() | binary(),options()) -> container().
new() ->
    new(<<>>, []).
new(Name) ->
    new(Name, []).
new(<<>>, Options) ->
    new(name(<<"erlxc">>), Options ++ [temporary]);
new(Name, Options) ->
    Port = erlxc_drv:start(Name, Options ++ [transient]),
    #container{port = Port}.

-spec connect(container()) -> container().
-spec connect(container(), options()) -> container().
connect(Container) ->
    connect(Container, []).
connect(#container{port = Port, console = undefined} = Container, Options) ->
    Name = liblxc:name(Port),
    Console = erlxc_console:start(Name, Options),
    Container#container{console = Console};
connect(#container{console = Console0} = Container, Options) ->
    catch erlxc_console:stop(Console0),
    connect(Container#container{console = undefined}, Options).

-spec config(container(), options()) -> 'true'.
config(#container{port = Port}, Options) ->
    Config = proplists:get_value(config, Options, []),

    Name = liblxc:name(Port),
    Path = liblxc:get_config_path(Port),

    Rootfs = case liblxc:get_config_item(Port, "lxc.rootfs") of
        false ->
            <<Path/binary, "/", Name/binary, "/rootfs">>;
        N ->
            N
    end,

    verbose(1, {path, [
                Path,
                liblxc:config_file_name(Port),
                Rootfs
            ]}, Options),

    call(Port, set_config_item, ["lxc.utsname", Name]),
    call(Port, set_config_item, ["lxc.rootfs", Rootfs]),

    lists:foreach(fun
            (<<>>) ->
                verbose(1, {clear_config, [Port]}, Options),
                call(Port, clear_config, []);
            ({load, File}) ->
                verbose(1, {load_config, [Port]}, Options),
                call(Port, load_config, [File]);
            ({Key, Value}) ->
                verbose(1, {set_config_item, [Port, Key, Value]}, Options),
                call(Port, set_config_item, [Key, Value]);
            (Key) ->
                verbose(1, {clear_config_item, [Port, Key]}, Options),
                call(Port, clear_config_item, [Key])
        end, Config),

    call(Port, save_config, [liblxc:config_file_name(Port)]).

-spec cgroup(container(), options()) -> 'true'.
cgroup(#container{port = Port}, Options) ->
    Cgroup = proplists:get_value(cgroup, Options, []),

    lists:foreach(fun
            ({Key, Value}) ->
                verbose(1, {set_cgroup_item, [Port, Key, Value]}, Options),
                call(Port, set_cgroup_item, [Key, Value])
        end, Cgroup),

    true.

-spec chroot(container(), options()) -> 'true'.
chroot(#container{port = Port}, Options) ->
    ConfigPath = proplists:get_value(path, Options, liblxc:get_config_path(Port)),
    Chroot = proplists:get_value(chroot, Options, []),

    Dir = proplists:get_value(dir, Chroot, []),
    Copy = proplists:get_value(copy, Chroot, []),
    File = proplists:get_value(file, Chroot, []),

    Name = liblxc:name(Port),
    Path = <<(maybe_binary(ConfigPath))/binary, "/", Name/binary, "/rootfs">>,

    make(dir, Path, Dir, Options),
    make(copy, Path, Copy, Options),
    make(file, Path, File, Options),

    true.

-spec create(container(), options()) -> 'true'.
create(#container{port = Port}, Options) ->
    Create = proplists:get_value(create, Options, []),

    Template = proplists:get_value(template, Create, <<"ubuntu">>),
    Bdevtype = proplists:get_value(bdevtype, Create, <<>>),
    Bdevspec = proplists:get_value(bdevspec, Create, <<>>),
    Flags = proplists:get_value(flags, Create, 0),
    Argv = proplists:get_value(argv, Create, []),

    verbose(1, {create, [Port, Template, Bdevtype, Bdevspec, Flags, Argv]}, Options),
    call(Port, create, [Template, Bdevtype, Bdevspec, Flags, Argv]).

-spec start(container(), options()) -> 'true'.
start(#container{port = Port}, Options) ->
    Start = proplists:get_value(start, Options, []),
    UseInit = proplists:get_value(useinit, Start, false),
    Argv = proplists:get_value(argv, Start, []),

    verbose(1, {start, [Port, UseInit, Argv]}, Options),
    call(Port, start, [bool(UseInit), Argv]).

%%--------------------------------------------------------------------
%%% Container state
%%--------------------------------------------------------------------

% "STOPPED", "STARTING", "RUNNING", "STOPPING",
% "ABORTING", "FREEZING", "FROZEN", "THAWED",
boot(Container, Options) ->
    state(Container, Options).

state(#container{port = Port} = Container, Options) ->
    state(Container, liblxc:state(Port), liblxc:defined(Port), Options).
state(Container, <<"STOPPED">>, false, Options) ->
    Chroot = proplists:is_defined(chroot, Options),
    case Chroot of
        true ->
            chroot(Container, Options);
        false ->
            create(Container, Options)
    end,
    config(Container, Options),
    start(Container, Options),
    state(Container, Options);
state(Container, <<"RUNNING">>, true, Options) ->
    cgroup(Container, Options),
    connect(Container, Options);
state(Container, <<"STOPPED">>, true, Options) ->
    config(Container, Options),
    start(Container, Options),
    state(Container, Options);
state(#container{port = Port} = Container, <<"FROZEN">>, true, Options) ->
    call(Port, unfreeze, []),
    state(Container, Options);
state(Container, _State, true, Options) ->
    timer:sleep(timer:seconds(5)),
    state(Container, Options).

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

-spec verbose(integer(),{atom(), list()}, proplists:proplist()) -> 'ok'.
verbose(Level, Msg, Opt) ->
    Verbose = proplists:get_value(verbose, Opt, 0),
    if
        Verbose >= Level ->
            error_logger:info_report([Msg, {options, Opt}]);
        true ->
            ok
    end.

-spec maybe_binary(file:filename_all()) -> binary().
maybe_binary(N) when is_list(N) -> list_to_binary(N);
maybe_binary(N) when is_binary(N) -> N.

-spec make('dir' | 'copy' | 'file', binary(), [file:filename_all() | tuple()], proplists:proplist()) -> 'ok'.
make(_Type, _Path, [], _Options) ->
    ok;
make(Type, Path, [Obj|Rest] = Files, Options) when Type =:= dir; Type =:= copy; Type =:= file ->
    verbose(1, {Type, [Path, Obj]}, Options),
    case ?MODULE:Type(Path, Obj) of
        ok ->
            make(Type, Path, Rest, Options);
        Error ->
            erlang:error({case_clause, Error}, [Type, Path, Files, Options])
    end.

-type filemode() :: file:filename_all() | integer() | 'undefined'.

-spec dir(binary(), file:filename_all() | {file:filename_all(), filemode()}) -> 'ok' | {'error', file:posix()}.
dir(Path, Dir) when is_binary(Dir); is_list(Dir) ->
    dir(Path, {Dir, undefined});
dir(Path, {Dir0, Info}) ->
    Dir = filename:absname_join(Path, Dir0),
    case filelib:ensure_dir(<<Dir/binary, "/">>) of
        ok ->
            write_file_info(Dir, Info);
        Error ->
            Error
    end.

-spec copy(binary(), {file:filename_all(), file:filename_all()} |
    {file:filename_all(), file:filename_all(), filemode()}) ->
    'ok' | {'error', file:posix()}.
copy(Path, {Source, Destination}) ->
    copy(Path, {Source, Destination, undefined});
copy(Path, {Source, Destination0, Info}) ->
    Destination = filename:absname_join(Path, Destination0),
    case file:copy(Source, Destination) of
        {ok, _} ->
            write_file_info(Destination, Info);
        Error ->
            Error
    end.

-spec file(binary(), {file:filename_all(), iodata()} |
    {file:filename_all(), iodata(), filemode()}) -> 'ok' | {'error', file:posix()}.
file(Path, {File, Content}) ->
    file(Path, {File, Content, undefined});
file(Path, File) when is_binary(File); is_list(File) ->
    file(Path, {File, <<>>, undefined});
file(Path, {File0, Content, Info}) ->
    File = filename:absname_join(Path, File0),
    case file:write_file(File, Content) of
        ok ->
            write_file_info(File, Info);
        Error ->
            Error
    end.

-spec write_file_info(file:filename_all(), filemode()) -> 'ok' | {'error', file:posix()}.
write_file_info(_File, undefined) ->
    ok;
write_file_info(File, #file_info{} = Info) ->
    file:write_file_info(File, Info);
write_file_info(File, Mode) when is_integer(Mode) ->
    file:write_file_info(File, #file_info{mode = Mode}).

call(Container, Call, Arg) ->
    case liblxc:call(Container, Call, Arg) of
        true ->
            true;
        false ->
            erlang:error({case_clause, false}, [Container, Call, Arg])
    end.
