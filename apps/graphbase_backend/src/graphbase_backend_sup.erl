%%%-------------------------------------------------------------------
%% @doc graphbase_backend top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_backend_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Host, Port, Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {Host, Port, Options}).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init({Host, Port, Options}) ->
    {ok, {{one_for_one, 0, 1}, [
        #{
            id => graphbase_backend_connection_pool_sup,
            start => {graphbase_backend_connection_pool, start_link, [Host, Port, Options]},
            restart => permanent,
            type => supervisor,
            modules => [graphbase_backend_connection_pool_sup]
        }
    ]}}.
