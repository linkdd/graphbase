%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver public API
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-record(state, {conn}).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = graphbase_apiserver_sup:start_link(),
    {ok, Conn} = graphbase_backend_connection_pool:acquire(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", graphbase_apiserver_root, []},
            {"/status", graphbase_apiserver_status, Conn},
            {"/api/requests", graphbase_apiserver_requests, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        graphbase_apiserver_listener,
        [{port, graphbase_core:get_confopt(int, port, 7439)}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, Pid, #state{conn = Conn}}.

%%--------------------------------------------------------------------
stop(#state{conn = Conn}) ->
    graphbase_backend_connection_pool:release(Conn).
