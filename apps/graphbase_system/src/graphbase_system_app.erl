%%%-------------------------------------------------------------------
%% @doc graphbase_system public API
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_system_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, initialize/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    initialize(),
    graphbase_system_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

initialize() ->
    {ok, Conn} = graphbase_backend_connection_pool:acquire(),
    SystemGraph0 = graphbase_system_metagraph:new(Conn),
    User0 = graphbase_system_user:new(Conn, SystemGraph0, <<"admin">>),
    SystemGraph1 = graphbase_system_metagraph:add_user(SystemGraph0, User0),
    R0 = graphbase_entity_obj:save(SystemGraph1),
    R1 = graphbase_entity_obj:save(User0),
    io:format("R0 =~n~p~nR1 = ~n~p~n", [R0, R1]),
    {{ok, _}, {ok, _}} = {R0, R1},
    graphbase_backend_connection_pool:release(Conn).
