%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver public API
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ListenerName = graphbase_apiserver_listener,
    Port = graphbase_core:get_confopt(int, apiserver_port, 7439),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", graphbase_apiserver_root, []},
            {"/status", graphbase_apiserver_status, []},
            {"/api/requests", graphbase_apiserver_requests, []}
        ]}
    ]),
    case start_listener(ListenerName, Port, #{env => #{dispatch => Dispatch}}) of
        {ok, Listener} ->
            {ok, Pid} = graphbase_apiserver_sup:start_link(),
            {ok, Pid, Listener};

        Error ->
            {error, {unable_to_start_listener, Error}}
    end.

%%--------------------------------------------------------------------
stop(Listener) ->
    cowboy:stop_listener(Listener).

%%====================================================================
%% Internal functions
%%====================================================================

start_listener(ListenerName, Port, Options) ->
    start_listener(graphbase_core:get_confopt(apiserver_tls, false), ListenerName, Port, Options).

%%--------------------------------------------------------------------
start_listener(true, ListenerName, Port, Options) ->
    case graphbase_core:get_confopt(str, apiserver_cacertfile, undefined) of
        undefined  -> {error, {missing_opt, cacertfile}};
        CACertFile ->
            case graphbase_core:get_confopt(str, apiserver_certfile, undefined) of
                undefined -> {error, {missing_opt, certfile}};
                CertFile  ->
                    cowboy:start_tls(
                        ListenerName,
                        [
                            {port, Port},
                            {cacertfile, CACertFile},
                            {certfile, CertFile}
                        ],
                        Options
                    )
            end
    end;

start_listener(false, ListenerName, Port, Options) ->
    cowboy:start_clear(ListenerName, [{port, Port}], Options).
