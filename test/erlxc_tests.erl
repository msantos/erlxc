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
-module(erlxc_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlxc/include/erlxc.hrl").
-include_lib("kernel/include/file.hrl").

erlxc_test_() ->
    {setup,
        fun startit/0,
        fun stopit/1,
        fun runit/1
    }.

runit(Container) ->
    [
        name(),
        chroot(Container),
        erlxc_exit(Container)
    ].

startit() ->
    Verbose = list_to_integer(getenv("ERLXC_TEST_VERBOSE", "0")),
    Bridge = getenv("ERLXC_TEST_BRIDGE", <<"lxcbr0">>),

    % XXX Generates the same config as above except adds duplicate
    % XXX lxc.network.link items, one with lxcbr0 and the other set to br0
    Config = [
        {verbose, Verbose},
        {config, [
            {<<"lxc.network.link">>, Bridge}
        ]}
    ],

    erlxc:spawn(<<>>, Config).

stopit(Container) ->
    erlxc_drv:stop(Container#container.port).

name() ->
    Template1 = <<"abcd####">>,
    Name1 = erlxc:name(Template1),
    <<"abcd", _:4/bytes>> = Name1,

    Template2 = <<"abcd1234">>,
    Template2 = erlxc:name(Template2),

    ?_assertNotMatch(Name1, Template1).

chroot(Container) ->
    Chroot = [
        {path, priv_dir()},
        {chroot, [
                {dir, [
                    "/tmp/erlxc",
                    "etc",
                    "bin",
                    {"home/test", #file_info{mode = 8#700}}
                ]},
                {copy, [
                    {"/etc/passwd", "etc/passwd"},
                    {"/bin/ls", "bin/ls", 8#755}
                ]},
                {file, [
                    "home/test/empty",
                    {"home/test/.profile", <<"export PATH=/sbin:/bin:/usr/sbin:/usr/bin">>},
                    {"home/test/true", <<>>, #file_info{mode = 8#755}}
                ]}
        ]}
    ],
    Reply = erlxc:chroot(Container, Chroot),
    ?_assertEqual(true, Reply).

erlxc_exit(Container) ->
    true = erlxc:exit(Container, kill),
    Reply = liblxc:running(Container#container.port),
    ?_assertEqual(false, Reply).

getenv(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        N -> N
    end.

priv_dir() ->
    Module = erlxc,
    case code:priv_dir(Module) of
        {error, bad_name} ->
            filename:join([
                filename:dirname(code:which(Module)),
                "..",
                "priv",
                "test"
            ]);
        Dir ->
            filename:join([Dir, "test"])
    end.
