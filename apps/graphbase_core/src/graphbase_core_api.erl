-module(graphbase_core_api).

%% API exports
-export([debug/1]).

%%====================================================================
%% API functions
%%====================================================================

debug(Arguments) ->
    io:format("~p~n", [Arguments]),
    {ok, debug}.
