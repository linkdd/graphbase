%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver public API
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-record(stateset, {user_col}).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = graphbase_apiserver_sup:start_link(),
    StateSet = #stateset{
        user_col = graphbase_apiserver_user_collection:init_state()
    },
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", graphbase_apiserver_root, []},
            {"/status", graphbase_apiserver_status, []},
            {"/api/users", graphbase_apiserver_user_collection, StateSet#stateset.user_col},
            {"/api/requests", graphbase_apiserver_requests, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        graphbase_apiserver_listener,
        [{port, graphbase_core:get_confopt(int, apiserver_port, 7439)}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, Pid}.

%%--------------------------------------------------------------------
stop(#stateset{user_col = UserColState}) ->
    graphbase_apiserver_user_collection:finalize_state(UserColState).
