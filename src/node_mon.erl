%%% 
%%% Copyright (c) 2008-2014 JackNyfe, Inc. <info@jacknyfe.com>
%%% All rights reserved.
%%%
%%% vim: set ts=4 sts=4 sw=4 et:

-module(node_mon).
-behavior(gen_server).
-export([
    start_link/2,
    start_link/3,
    status/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(INITIAL_LOCAL_TIMEOUT, 10000).
-define(RETRY_TIMEOUT, 60000).

start_link(Node, ParentPid) ->
    start_link(Node, ParentPid, []).

start_link(Node, ParentPid, Options) ->
    gen_server:start_link(?MODULE, [Node, ParentPid, Options], []).

status(ServerRef) -> gen_server:call(ServerRef, status).

-record(state, {node, parent, status = unknown, timer, options }).

init([Node, ParentPid, Options]) ->
    {ok, #state{ node = Node, parent = ParentPid,
        timer = erlang:send_after(initial_timeout(Node), self(), timed_retry),
        options = Options
    }}.

handle_call(status, _From, State) ->
    {reply, [{status, State#state.status}], State}.

handle_cast(_, State) -> {noreply, State}.

check_status(Node, true = _PingOnly) ->
    case jsk_async:wait(jsk_async:run(fun() -> net_adm:ping(Node) end), 15000) of
        {ok, pong} -> up;
        _  -> down
    end;
check_status(Node, _PingOnly) ->
    case rpc:call(Node, node_status, get, [], 15000) of
        up -> up;
        {badrpc, {'EXIT', {undef, [{node_status,get,[]}|_]}}} ->
            % Node alive, but no node_status module is loaded.
            % Perhaps, not JS-Kit node (riak?)
            up;
        {badrpc, {'EXIT', {noproc, {gen_server, _, _}}}} ->
            % node_status server has not started (infant node)
            down;
        _ ->
            % nodedown, still initializing, or something else.
            down
    end.

handle_info(timed_retry, #state{ node = Node, options = Options } = OldState) ->
    State = OldState#state{timer = undefined},
    Status = check_status(Node, proplists:get_value(ping_only, Options, false)),
    {noreply, change_status(Status, State)};

handle_info({nodedown, Node}, #state{ node = Node } = State) ->
    {noreply, change_status(down, State)};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions

change_status(NewStatus, #state{node = Node, parent = Pid, status = OldStatus} = State) ->
    NewState = case NewStatus of
        up   -> monitor_node(Node, true), stop_timer(State);
        down -> start_timer_if_not_running(State)
    end,
    case NewStatus == OldStatus of
        true  -> ignore;
        false -> Pid ! {?MODULE, Node, NewStatus}
    end,
    NewState#state{status = NewStatus}.

stop_timer(#state{timer = undefined} = State) -> State;
stop_timer(#state{timer = TRef} = State) ->
    erlang:cancel_timer(TRef),
    State#state{timer=undefined}.

start_timer_if_not_running(#state{timer = TRef} = State)
        when is_reference(TRef) ->
    State;
start_timer_if_not_running(#state{timer = undefined, options = Opts} = State) ->
    Timeout = proplists:get_value(timeout, Opts, ?RETRY_TIMEOUT),
    State#state{timer=erlang:send_after(Timeout, self(), timed_retry)}.

initial_timeout(Node) when Node == node() -> ?INITIAL_LOCAL_TIMEOUT;
initial_timeout(_) -> 1000.
