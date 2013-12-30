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
-module(liblxc_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

% lxc_container_clear_config/1
% lxc_container_set_config_item/3
% lxc_container_load_config/2
% lxc_container_init_pid/1
% argv/1

liblxc_test_() ->
    {setup,
        fun startit/0,
        fun stopit/1,
        fun runit/1
    }.

runit(Container) ->
    [
        version(Container),

        create(Container),

        defined(Container),
        state(Container),
        wait(Container),

        list(Container, active),
        list(Container, all),
        list(Container, defined),

        start(Container),

        running(Container),
        name(Container),
        config_file_name(Container),
        ?MODULE:get_keys(Container),
        get_config_item(Container),
        clear_config_item(Container),
        get_config_path(Container),
        set_config_path(Container),

        set_cgroup_item(Container),
        get_cgroup_item(Container),

        save_config(Container),

        freeze(Container),
        unfreeze(Container),

        stop(Container),

        destroy(Container)
    ].

startit() ->
    N = binary:decode_unsigned(crypto:rand_bytes(1)),
    Name = <<"liblxc", (i2b(N))/binary>>,
    Container = erlxc_drv:start(Name),
    Container.

stopit(Container) ->
    erlxc_drv:stop(Container).

version(Container) ->
    Reply = is_binary(liblxc:version(Container)),
    ?_assertEqual(true, Reply).

create(Container) ->
    Bridge = getenv("ERLXC_TEST_BRIDGE", <<"lxcbr0">>),
    false = liblxc:defined(Container),
    false = liblxc:running(Container),
    true = liblxc:set_config_item(Container, <<"lxc.network.type">>, <<"veth">>),
    true = liblxc:set_config_item(Container, <<"lxc.network.link">>, Bridge),
    true = liblxc:set_config_item(Container, <<"lxc.network.flags">>, <<"up">>),

    Reply = liblxc:create(
        Container,
        <<"ubuntu">>,
        <<>>,
        <<>>,
        0,
        [<<"-r">>, <<"precise">>]
    ),

    ?_assertEqual(true, Reply).

destroy(Container) ->
    Reply = liblxc:destroy(Container),
    ?_assertEqual(true, Reply).

start(Container) ->
    Reply = liblxc:start(Container),
    ?_assertMatch(true, Reply).

stop(Container) ->
    Reply = liblxc:stop(Container),
    ?_assertMatch(true, Reply).

list(Container, Type) ->
    Reply = liblxc:list(Container, Type),
    ?_assertEqual(true, is_list(Reply)).

name(Container) ->
    Reply = liblxc:name(Container),
    ?_assertMatch(<<"liblxc", _/binary>>, Reply).

config_file_name(Container) ->
    Reply = liblxc:config_file_name(Container),
    ?_assertEqual(true, is_binary(Reply)).

get_keys(Container) ->
    Bin = liblxc:get_keys(Container),
    Keys = binary:split(Bin, <<"\n">>, [global,trim]),
    Exists = lists:member(<<"lxc.utsname">>, Keys),
    ?_assertEqual(true, Exists).

get_config_item(Container) ->
    Reply = liblxc:get_config_item(Container, <<"lxc.utsname">>),
    ?_assertMatch(<<"liblxc", _/binary>>, Reply).

clear_config_item(Container) ->
    Item = liblxc:get_config_item(Container, <<"lxc.network">>),
    true = is_binary(Item),
    true = liblxc:clear_config_item(Container, <<"lxc.network">>),
    true = liblxc:clear_config_item(Container, <<"lxc.network">>),
    Reply = liblxc:get_config_item(Container, <<"lxc.network">>),
    ?_assertEqual(none, Reply).

state(Container) ->
    Reply = liblxc:state(Container),
    ?_assertEqual(<<"STOPPED">>, Reply).

wait(Container) ->
    State = liblxc:state(Container),
    % Will timeout if wait fails
    Reply = liblxc:wait(Container, State, 60 * 10),
    ?_assertEqual(true, Reply).

defined(Container) ->
    Reply = liblxc:defined(Container),
    ?_assertEqual(true, Reply).

running(Container) ->
    Reply = liblxc:running(Container),
    ?_assertEqual(true, Reply).

get_config_path(Container) ->
    Reply = liblxc:get_config_path(Container),
    ?_assertEqual(true, is_binary(Reply)).

set_config_path(Container) ->
    Path = liblxc:get_config_path(Container),
    Reply = liblxc:set_config_path(Container, Path),
    ?_assertEqual(true, Reply).

set_cgroup_item(Container) ->
    Reply = liblxc:set_cgroup_item(Container, "cpu.shares", <<"10">>),
    ?_assertEqual(true, Reply).

get_cgroup_item(Container) ->
    Reply = liblxc:get_cgroup_item(Container, "cpu.shares"),
    ?_assertEqual(<<"10\n">>, Reply).

save_config(Container) ->
    Name = liblxc:name(Container),
    Cfg = <<"/tmp/", Name/binary, ".cfg">>,
    Reply = liblxc:save_config(Container, Cfg),
    {ok,_} = file:read_file_info(Cfg),
    file:delete(Cfg),
    ?_assertEqual(true, Reply).

freeze(Container) ->
    true = liblxc:freeze(Container),
    Reply = liblxc:state(Container),
    ?_assertEqual(<<"FROZEN">>, Reply).

unfreeze(Container) ->
    true = liblxc:unfreeze(Container),
    Reply = liblxc:state(Container),
    ?_assertEqual(<<"RUNNING">>, Reply).

i2b(N) ->
    list_to_binary(integer_to_list(N)).

getenv(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        N -> N
    end.
