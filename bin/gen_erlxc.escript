#!/usr/bin/env escript

%%%
%%% Generate the erlxc.erl file
%%%
main([]) ->
    File = "erlxc.erl",
    Proto = "c_src/erlxc_cmd.proto",
    main([File, Proto]);

main([File, Proto]) ->
    mkerl(File, Proto).

license() ->
    {{Year,_,_},{_,_,_}} = calendar:universal_time(),

    Date = integer_to_list(Year),

    License = [
" Copyright (c) " ++ Date ++ ", Michael Santos <michael.santos@gmail.com>",
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
    Body = erl_syntax:tuple([ erl_syntax:atom(N) || {N,_,_} <- Calls ]),
    Clause = erl_syntax:clause(Pattern, [], [Body]),
    [erl_syntax:function(erl_syntax:atom("api"), [Clause])].

mkerl(File, Proto) ->
    Module = erl_syntax:attribute(
            erl_syntax:atom(module),
            [erl_syntax:atom(filename:basename(File, ".erl"))]
            ),
%    Includes = includes(["erlxc.hrl"]),

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
                        || {Fun, _, Arity} <- Calls ])
                ]),

    % Generate the functions
    Functions = [ begin
                    {Pattern, Body} = case UseCID of
                        true ->
                            % name(Ref, Container, ...) -> erlxc:call(Ref, Fun, [Container, ...])
                            Arg = arg("Arg", Arity-1),
                            P = [erl_syntax:variable("Ref"), erl_syntax:variable("Container")|Arg],
                            B = erl_syntax:application(
                                    erl_syntax:atom(erlxc),
                                    erl_syntax:atom(call),
                                    [erl_syntax:variable("Ref"), erl_syntax:atom(Fun),
                                       erl_syntax:list([erl_syntax:variable("Container")|Arg])]),
                            {P,B};

                        false ->
                            % name(Ref, Container, ...) -> erlxc:call(Ref, Fun, [...])
                            Arg = arg("Arg", Arity),
                            P = [erl_syntax:variable("Ref")|Arg],
                            B = erl_syntax:application(
                                    erl_syntax:atom(erlxc),
                                    erl_syntax:atom(call),
                                    [erl_syntax:variable("Ref"), erl_syntax:atom(Fun),
                                       erl_syntax:list(Arg)]),
                            {P,B}
                    end,

                    Clause = erl_syntax:clause(Pattern, [], [Body]),
                    erl_syntax:function(erl_syntax:atom(Fun), [Clause])
                end || {Fun, UseCID, Arity} <- Calls ],

    Code0 = erl_prettypr:format(erl_syntax:form_list(lists:flatten([
                license(),
                Module,
%                Includes,

                Comment_static,
                Exports_static,

                Comment_gen,
                Exports_gen,

                Static,
                api(Proto),
                Functions
            ]))),

    Code = re:replace(Code0, "%%__STATIC__%%", static()),

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
    {Name, UseCID} = case Fun of
        <<"lxc_container_new">> ->
            {<<"new">>, false};
        <<"lxc_container_", Rest/binary>> ->
            {Rest, true};
        _ ->
            {Fun, false}
    end,
    call_to_fun(T, [{binary_to_list(Name), UseCID, binary_to_integer(Arity)}|Acc]).

static_exports() ->
    [{list,1},
     {list,2},
     {list,3},
     {new,2},
     {command,1},
     {call,2},
     {call,3}].

static() ->
    [ static({Fun, Arity}) || {Fun, Arity} <- static_exports() ].

static({list,1}) ->
"
list(Ref) ->
    list(Ref, all).
";

static({list,2}) ->
"
list(Ref, Type) ->
    list(Ref, Type, [<<>>]).
";

static({list,3}) ->
"
list(Ref, Type, Path) when Type =:= all; Type =:= active; Type =:= defined ->
    Cmd = \"list_\" ++ atom_to_list(Type) ++ \"_containers\",
    call(Ref, list_to_atom(Cmd), [Path]).
";

static({new,2}) ->
"
new(Ref, Name) ->
    new(Ref, Name, <<>>).
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
call(Pid, Command) ->
    call(Pid, Command, []).
";
static({call,3}) ->
"
call(Pid, Command, Arg) when is_pid(Pid) ->
    erlxc_drv:call(Pid, erlxc_drv:encode(command(Command), Arg)).
".

%includes(Header) ->
%    [ erl_syntax:attribute(erl_syntax:atom(include), [erl_syntax:string(N)]) || N <- Header ].