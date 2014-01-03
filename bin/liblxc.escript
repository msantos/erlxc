#!/usr/bin/env escript

%%%
%%% Generate the liblxc.erl file
%%%
main([]) ->
    File = "liblxc.erl",
    Proto = "c_src/erlxc_cmd.proto",
    main([File, Proto]);

main([File, Proto]) ->
    mkerl(File, Proto).

license() ->
    {{Year,_,_},{_,_,_}} = calendar:universal_time(),

    Date = integer_to_list(Year),

    License = [
" Copyright (c) 2013-" ++ Date ++ ", Michael Santos <michael.santos@gmail.com>",
" Permission to use, copy, modify, and/or distribute this software for any",
" purpose with or without fee is hereby granted, provided that the above",
" copyright notice and this permission notice appear in all copies.",
"",
" THE SOFTWARE IS PROVIDED \"AS IS\" AND THE AUTHOR DISCLAIMS ALL WARRANTIES",
" WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF",
" MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR",
" ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES",
" WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN",
" ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF",
" OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE."],

    erl_syntax:comment(License).

api(Proto) ->
    Calls = calls(Proto),


    % Generate the function
    Pattern = [],
    Body = erl_syntax:tuple([ erl_syntax:atom(N) || {N,_} <- Calls ]),
    Clause = erl_syntax:clause(Pattern, [], [Body]),
    [erl_syntax:function(erl_syntax:atom("api"), [Clause])].

mkerl(File, Proto) ->
    Module = erl_syntax:attribute(
            erl_syntax:atom(module),
            [erl_syntax:atom(filename:basename(File, ".erl"))]
            ),
%    Includes = includes(["liblxc.hrl"]),

    % Type specs
    Specs = erl_syntax:comment(["%__SPECS__%%"]),

    % Any hardcoded functions will be included here
    Static = erl_syntax:comment(["%__STATIC__%%"]),

    Calls = calls(Proto),

    % Generate the list of exports
    Comment_static = erl_syntax:comment([" Static functions"]),
    Exports_static = erl_syntax:attribute(erl_syntax:atom(export), [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity))
                        || {Fun, Arity} <- static_exports() ])
                ]),

    Comment_gen = erl_syntax:comment([" Generated functions"]),
    Exports_gen = erl_syntax:attribute(erl_syntax:atom(export), [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity+1))
                        || {Fun, Arity} <- Calls ])
                ]),

    % Generate the functions
    Functions = [ begin
                    % name(Container, ...) -> liblxc:call(Container, Fun, [...])
                    Arg = arg("Arg", Arity),
                    Pattern = [erl_syntax:variable("Container")|Arg],
                    Body = erl_syntax:application(
                        erl_syntax:atom(call),
                        [erl_syntax:variable("Container"), erl_syntax:atom(Fun),
                            erl_syntax:list(Arg)]
                    ),
                    Clause = erl_syntax:clause(Pattern, [], [Body]),
                    erl_syntax:function(erl_syntax:atom(Fun), [Clause])
                end || {Fun, Arity} <- Calls ],

    Code0 = erl_prettypr:format(erl_syntax:form_list(lists:flatten([
                license(),
                Module,
%                Includes,

                Specs,

                Comment_static,
                Exports_static,

                Comment_gen,
                Exports_gen,

                Static,
                api(Proto),
                Functions
            ]))),

    Code1 = re:replace(Code0, "%%__STATIC__%%", static()),
    Code = re:replace(Code1, "%%__SPECS__%%", specs()),

%    io:format("~s~n", [Code]).
    file:write_file(File, [Code]).

arg(Prefix, Arity) ->
    [ erl_syntax:variable(string:concat(Prefix, integer_to_list(N))) || N <- lists:seq(1,Arity) ].

% List the supported liblxc API functions
calls(Proto) ->
    {ok, Bin} = file:read_file(Proto),
    Fun = binary:split(Bin, <<"\n">>, [trim,global]),
    call_to_fun(Fun, []).

call_to_fun([], Acc) ->
    lists:reverse(Acc);
call_to_fun([H|T], Acc) ->
    [Fun, Arity] = binary:split(H, <<"/">>),
    Name = case Fun of
        <<"lxc_container_", Rest/binary>> ->
            Rest;
        _ ->
            Fun
    end,
    call_to_fun(T, [{binary_to_list(Name), binary_to_integer(Arity)}|Acc]).

static_exports() ->
    [{list,1},
     {list,2},
     {list,3},
     {load_config,1},
     {start,1},
     {get_keys,1},
     {command,1},
     {call,2},
     {call,3}].

static() ->
    [ static({Fun, Arity}) || {Fun, Arity} <- static_exports() ].

static({list,1}) ->
"
list(Container) ->
    list(Container, all).
";

static({list,2}) ->
"
list(Container, Type) ->
    list(Container, Type, <<>>).
";

static({list,3}) ->
"
list(Container, Type, Path) when Type =:= all; Type =:= active; Type =:= defined ->
    Cmd = \"list_\" ++ atom_to_list(Type) ++ \"_containers\",
    call(Container, list_to_atom(Cmd), [Path]).
";

static({load_config,1}) ->
"
load_config(Container) ->
    load_config(Container, <<>>).
";

static({start,1}) ->
"
start(Container) ->
    start(Container, 0, []).
";

static({get_keys,1}) ->
"
get_keys(Container) ->
    get_keys(Container, <<>>).
";

static({command,1}) ->
"
command(Cmd) when is_atom(Cmd) ->
    lookup(Cmd, api()).

lookup(Cmd, Cmds) ->
    lookup(Cmd, 1, Cmds, tuple_size(Cmds)).
lookup(Cmd, N, Cmds, _Max) when Cmd =:= element(N, Cmds) ->
    % Convert to 0 offset
    N-1;
lookup(Cmd, N, Cmds, Max) when N =< Max ->
    lookup(Cmd, N+1, Cmds, Max).
";

static({call,2}) ->
"
call(Container, Command) ->
    call(Container, Command, []).
";
static({call,3}) ->
"
call(Container, Command, Arg) when is_port(Container), is_list(Arg) ->
    erlxc_drv:call(Container, erlxc_drv:encode(command(Command), Arg)).
".

%includes(Header) ->
%    [ erl_syntax:attribute(erl_syntax:atom(include), [erl_syntax:string(N)]) || N <- Header ].

% FIXME hack for hard coding typespecs
specs() ->
"
-spec async_state_notify(port(), non_neg_integer()) -> boolean().
-spec async_state_close(port()) -> boolean().
-spec list_active_containers(port(), iodata()) -> [binary()].
-spec list_all_containers(port(), iodata()) -> [binary()].
-spec list_defined_containers(port(), iodata()) -> [binary()].
-spec clear_config(port()) -> 'true'.
-spec clear_config_item(port(), iodata()) -> boolean().
-spec config_file_name(port()) -> binary().
-spec create(port(), iodata(), iodata(), iodata(), integer(), [binary()]) -> boolean().
-spec defined(port()) -> boolean().
-spec destroy(port()) -> boolean().
-spec freeze(port()) -> boolean().
-spec get_config_item(port(), iodata())  -> binary() | 'none'.
-spec get_cgroup_item(port(), iodata())  -> binary() | 'none'.
-spec get_config_path(port()) -> binary().
-spec get_keys(port(), iodata()) -> binary().
-spec init_pid(port()) -> integer().
-spec load_config(port(), iodata()) -> boolean().
-spec name(port()) -> binary().
-spec opt(port()) -> integer().
-spec permanent(port()) -> boolean().
-spec running(port()) -> boolean().
-spec save_config(port(), iodata()) -> boolean().
-spec set_config_item(port(), iodata(), iodata()) -> boolean().
-spec set_cgroup_item(port(), iodata(), iodata()) -> boolean().
-spec set_config_path(port(), iodata()) -> boolean().
-spec shutdown(port(), non_neg_integer()) -> boolean().
-spec start(port(), 0 | 1, [binary()]) -> boolean().
-spec state(port()) -> binary().
-spec stop(port()) -> boolean().
-spec temporary(port()) -> boolean().
-spec transitory(port()) -> boolean().
-spec type(port()) -> permanent | transitory | temporary.
-spec unfreeze(port()) -> boolean().
-spec wait(port(), iodata(), integer()) -> boolean().
-spec test_argv(port(), [binary()]) -> true.
-spec version(port()) -> binary().
".
