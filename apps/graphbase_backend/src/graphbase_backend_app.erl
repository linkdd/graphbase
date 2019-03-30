%%%-------------------------------------------------------------------
%% @doc graphbase_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_backend_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Host = graphbase_core:get_confopt(str, backend_riak_host, "127.0.0.1"),
    Port = graphbase_core:get_confopt(int, backend_riak_port, 8087),
    Options = graphbase_core:get_confopt(backend_riak_options, []),
    graphbase_backend_sup:start_link(Host, Port, Options).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
