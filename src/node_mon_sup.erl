-module(node_mon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type,Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Params = case application:get_env(start_expectations) of
        {ok,L} when is_list(L) -> L;
        _ -> []
    end,
    {ok, { {one_for_one, 5, 10}, [?CHILD(node_status,worker,[Params])]} }.

