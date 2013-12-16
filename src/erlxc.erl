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
-module(erlxc).

%% API
-export([
        new/2, new/3,
        start/2, start/4,
        stop/2,
        get_config_item/3,
        set_config_item/4,
        load_config/2, load_config/3,
        list/1, list/2, list/3,

        argv/2,

        call/2, call/3
    ]).

argv(Ref, N) when is_list(N) ->
    call(Ref, argv, [N]).

new(Ref, Name) ->
    new(Ref, Name, <<>>).
new(Ref, Name, Path) ->
    call(Ref, lxc_container_new, [Name, Path]).

start(Ref, C) ->
    start(Ref, C, 0, []).
start(Ref, C, Useinit, Argv) ->
    call(Ref, lxc_container_start, [C, Useinit, Argv]).

stop(Ref, C) ->
    call(Ref, lxc_container_stop, [C]).

get_config_item(Ref, C, Key) ->
    call(Ref, lxc_container_get_config_item, [C, Key]).

set_config_item(Ref, C, Key, Val) ->
    call(Ref, lxc_container_set_config_item, [C, Key, Val]).

load_config(Ref, C) ->
    load_config(Ref, C, <<>>).
load_config(Ref, C, Path) ->
    call(Ref, lxc_container_load_config, [C, Path]).

list(Ref) ->
    list(Ref, all).
list(Ref, Type) ->
    list(Ref, Type, [<<>>]).
list(Ref, Type, Path) when Type =:= all; Type =:= active; Type =:= defined ->
    Cmd = "list_" ++ atom_to_list(Type) ++ "_containers",
    call(Ref, list_to_atom(Cmd), [Path]).

call(Pid, Command) ->
    call(Pid, Command, []).
call(Pid, Command, Arg) when is_pid(Pid) ->
    erlxc_drv:call(Pid, erlxc_drv:encode(Command, Arg)).
