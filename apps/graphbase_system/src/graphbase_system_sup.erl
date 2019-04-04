%%%-------------------------------------------------------------------
%% @doc graphbase_system top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_system_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {{one_for_all, 1, 5}, []}}.
