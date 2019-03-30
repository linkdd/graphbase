%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver Packet module
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_packet).

%% Application callbacks
-export([decode/1, encode/1]).

%%====================================================================
%% API
%%====================================================================

decode(Packet) ->
    binary_to_list(Packet).

%%--------------------------------------------------------------------
encode(Packet) ->
    list_to_binary(lists:flatten(io_lib:format("~p.", [Packet]))).
