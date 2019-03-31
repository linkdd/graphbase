%%%-------------------------------------------------------------------
%% @doc graphbase_system public API
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_system_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    graphbase_system_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.
