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
-behaviour(gen_server).

%% API
-export([start/0, start/1, start/2, stop/1]).
-export([start_link/2]).
-export([call/2]).
-export([encode/2]).
-export([getopts/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(ERLXC_COMMAND, {
    list_active_containers,
    list_all_containers,
    list_defined_containers,

    lxc_container_new,
    lxc_container_start,
    lxc_container_stop,
    lxc_container_set_config_item,
    lxc_container_load_config,

    argv
}).

-record(state, {pid :: pid(), port :: port()}).

start() ->
    start_link(self(), []).
start(Options) ->
    start_link(self(), Options).
start(Pid, Options) when is_pid(Pid), is_list(Options) ->
    start_link(Pid, Options).

start_link(Pid, Options) ->
    gen_server:start_link(?MODULE, [Pid, Options], []).

call(Pid, Data) when is_pid(Pid), is_binary(Data), byte_size(Data) < 16#ffff ->
    Reply = case gen_server:call(Pid, {call, Data}, infinity) of
        ok ->
            receive
                {erlxc, Pid, Msg} ->
                    binary_to_term(Msg)
            end;
        Error ->
            Error
    end,
    case Reply of
        %badarg -> erlang:error(badarg);
        badarg -> badarg;
        _ -> Reply
    end.

encode(Command, Arg) when is_atom(Command) ->
    encode(command(Command), Arg);
encode(Command, Arg) when is_integer(Command), is_list(Arg) ->
    <<Command:4/unsigned-integer-unit:8, (term_to_binary(Arg))/binary>>.

stop(Pid) ->
    gen_server:call(Pid, stop).

init([Pid, Options]) ->
    process_flag(trap_exit, true),
    Cmd = getopts(Options),
    Port = open_port({spawn, Cmd}, [{packet, 4}, binary]),
    {ok, #state{pid = Pid, port = Port}}.

handle_call({call, Packet}, _From, #state{port = Port} = State) ->
    Reply = try erlang:port_command(Port, Packet) of
        true ->
            ok
        catch
            error:badarg ->
                {error,closed}
        end,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
    catch erlang:port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Port communication
%%--------------------------------------------------------------------
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {shutdown, Reason}, State};
handle_info({Port, {data, Data}}, #state{port = Port, pid = Pid} = State) ->
    Pid ! {erlxc, self(), Data},
    {noreply, State};

% WTF
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
command(Cmd) when is_atom(Cmd) ->
    lookup(Cmd, ?ERLXC_COMMAND).

lookup(Cmd, Cmds) ->
    lookup(Cmd, 1, Cmds, tuple_size(Cmds)).
lookup(Cmd, N, Cmds, _Max) when Cmd =:= element(N, Cmds) ->
    % Convert to 0 offset
    N-1;
lookup(Cmd, N, Cmds, Max) when N =< Max ->
    lookup(Cmd, N+1, Cmds, Max).

getopts(Options) when is_list(Options) ->
    Exec = proplists:get_value(exec, Options, "sudo"),
    Progname = proplists:get_value(progname, Options, progname()),

    Switches = [ optarg(Opt) || Opt <- proplists:compact(Options) ],
    Cmd = [ N || N <- [Exec, Progname|Switches], N /= ""],

    string:join(Cmd, " ").

optarg({verbose, Arg})      -> string:copies("-v ", Arg);
optarg(_)                   -> "".

%switch(Switch, Arg) ->
%    lists:concat([Switch, " ", Arg]).

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
    filename:join([basedir("erlxc"), "erlxc"]).
