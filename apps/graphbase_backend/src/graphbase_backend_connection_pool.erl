%%%-------------------------------------------------------------------
%% @doc graphbase_backend connection pool supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_backend_connection_pool).

-behaviour(supervisor).

%% API
-export([
    start_link/3,
    acquire/0,
    release/1,
    with/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Host, Port, Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {Host, Port, Options}).

%%--------------------------------------------------------------------
acquire() ->
    supervisor:start_child(?SERVER, []).

%%--------------------------------------------------------------------
release(Conn) ->
    graphbase_backend_connection:stop(Conn).

%%--------------------------------------------------------------------
with(Block) ->
    graphbase_core:with(
        fun() -> graphbase_backend_connection_pool:acquire() end,
        fun(Conn) -> graphbase_backend_connection_pool:release(Conn) end,
        Block
    ).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init({Host, Port, Options}) ->
    {ok, {{simple_one_for_one, 0, 1}, [
        #{
            id => graphbase_backend_connection_instance,
            start => {graphbase_backend_connection, start_link, [Host, Port, Options]},
            restart => transient,
            type => worker,
            modules => [graphbase_backend_connection]
        }
    ]}}.
