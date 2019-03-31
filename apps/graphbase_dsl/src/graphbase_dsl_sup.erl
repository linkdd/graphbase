%%%-------------------------------------------------------------------
%% @doc graphbase_dsl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_sup).

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
    {ok, {{one_for_one, 1, 5}, [
        #{
            id => graphbase_dsl_api_server,
            start => {graphbase_dsl_api, start_link, []},
            restart => permanent,
            type => worker,
            modules => [graphbase_dsl_api]
        },
        #{
            id => graphbase_dsl_ast_server,
            start => {graphbase_dsl_ast, start_link, []},
            restart => permanent,
            type => worker,
            modules => [graphbase_dsl_ast]
        },
        #{
            id => graphbase_dsl_interpreter_server,
            start => {graphbase_dsl_interpreter, start_link, []},
            restart => permanent,
            type => worker,
            modules => [graphbase_dsl_interpreter]
        }
    ]}}.
