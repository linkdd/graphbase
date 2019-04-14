%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver Packet module
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_packet).

%% Application callbacks
-export([encode/1]).

%%====================================================================
%% API
%%====================================================================

encode(Packet) ->
    list_to_binary(lists:flatten(io_lib:format("~p.", [Packet]))).
