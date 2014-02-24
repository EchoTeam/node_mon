%%% 
%%% Copyright (c) 2008-2014 JackNyfe, Inc. <info@jacknyfe.com>
%%% All rights reserved.
%%%
%%% vim: set ts=4 sts=4 sw=4 et:

-module(node_status).
-behavior(gen_server).
-export([
        % public API
        expect/1,
        expect/2,
        get/0,
        get/1,
        start_link/0,
        start_link/1,
        start_link/2,
        status/0,
        status/1,
        stop/0,
        stop/1,
        trigger/1,
        trigger/2,
        trigger_if_recent/2,
        trigger_if_recent/3,

        % gen_server callbacks
        code_change/3,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        init/1,
        terminate/2
    ]).

%% The module is initialized with intention of being in the transitional
%% (initializing) state until all expectations are met.
%% Since some modules which affect node status may finish their initialization
%% much later thann node_status, we should list the future expectations upfront,
%% when node_status is initializing.
%%
%% EXAMPLE:
%%
%% node_status:start_link([queue1_recent, yaws_up]).
%% node_status:trigger(queue1_recent).
%%   initializing = node_status:get().
%% node_status:trigger(some_stray_event). % Ignored
%%   initializing = node_status:get().
%% node_status:trigger(yaws_up).
%%   up = node_status:get().
%%
start_link() -> start_link(jcfg:val(node_status_expectations)).
start_link(Expectations) ->
    start_link(Expectations, ?MODULE).
start_link(Expectations, Name) when is_list(Expectations), is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Expectations, []).

get() -> 
    ?MODULE:get(?MODULE).
get(Name) when is_atom(Name) -> gen_server:call(Name, get_status).

trigger(Event) -> trigger(Event, ?MODULE).
trigger(Event, Name) when is_atom(Name) -> gen_server:cast(Name, {trigger, Event}).

expect(Event) -> expect(Event, ?MODULE).
expect(Event, Name) when is_atom(Name) -> gen_server:cast(Name, {expect, Event}).

trigger_if_recent(Timestamp, Event) ->
    trigger_if_recent(Timestamp, Event, ?MODULE).
trigger_if_recent(Timestamp, Event, Name) when is_atom(Name) ->
    case timer:now_diff(now(), Timestamp) of
        MicroSecs when MicroSecs < 10000000 -> trigger(Event, Name);
        _ -> ok    
    end.

status() -> status(?MODULE).
status(Name) when is_atom(Name) -> gen_server:call(Name, status).

stop() -> stop(?MODULE).
stop(Name) when is_atom(Name) -> gen_server:call(Name, stop).

-record(state, {
        unmet_expectations = [],
        status = initializing % reinitializing | up
    }).

init(Expectations) ->
    {ok, #state{unmet_expectations = lists:sort(Expectations)}}.

handle_call(get_status, _From, #state{status = Status} = State) ->
    {reply, Status, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(status, _From, State) ->
    {reply, [
            {status, State#state.status},
            {unmet_expectations, State#state.unmet_expectations}
        ], State}.

handle_cast({trigger, _}, #state{unmet_expectations = []} = State) ->
    {noreply, State};
handle_cast({trigger, Event}, #state{unmet_expectations = Exp} = State) ->
    NewState = case Exp -- [Event] of
        Exp -> State; % No change
        [] ->
            io:format("~nNode ~p up, got ~p, no more expectations~n~n", [node(), Event]),
            State#state{status = up, unmet_expectations = []};
        NewExp ->
            io:format("~nNode ~p initializing, got ~p, still expecting ~p~n~n", [node(), Event, NewExp]),
            State#state{unmet_expectations = NewExp}
    end,
    {noreply, NewState};
handle_cast({expect, Event}, #state{unmet_expectations = Exp} = State) ->
    NewExp = lists:umerge([Event], Exp),
    case State#state.status == up of
        false -> ok;
        true -> io:format("~nNode ~p expects additional event: ~p~n~n",
                [node(), Event])
    end,
    {noreply, State#state{unmet_expectations = NewExp,
            status = reinitializing}};
handle_cast(_, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

node_status_test() ->
    Name = node_status_testing,
    {ok, _Pid} = node_status:start_link([queue1_recent, yaws_up], Name),
    node_status:trigger(queue1_recent, Name),
    initializing = node_status:get(Name),
    node_status:trigger(some_stray_event, Name), % Ignored
    initializing = node_status:get(Name),
    node_status:trigger(yaws_up, Name),
    up = node_status:get(Name),
    node_status:expect(some_more, Name),
    reinitializing = node_status:get(Name),
    node_status:trigger(some_more, Name),
    up = node_status:get(Name),
    node_status:stop(Name),
    ok.

-endif.
