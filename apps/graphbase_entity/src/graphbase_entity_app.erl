%%%-------------------------------------------------------------------
%% @doc graphbase_entity public API
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_entity_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-record(state, {conn}).

%%====================================================================
%% API Functions
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Conn} = graphbase_backend_connection_pool:acquire(),
    {ok, Pid} = graphbase_entity_sup:start_link(Conn),
    {ok, Pid, #state{conn = Conn}}.

%%--------------------------------------------------------------------
stop(#state{conn = Conn}) ->
    graphbase_backend_connection_pool:release(Conn).
