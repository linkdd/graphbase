%%%-------------------------------------------------------------------
%% @doc graphbase_entity top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_entity_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Conn) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Conn).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(Conn) ->
    {ok, {{one_for_one, 0, 1}, [
        #{
            id => graphbase_entity_api_server,
            start => {graphbase_entity_api, start_link, [Conn]},
            restart => permanent,
            type => worker,
            modules => [graphbase_entity_api]
        }
    ]}}.
