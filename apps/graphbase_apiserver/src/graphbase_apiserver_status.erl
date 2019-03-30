%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver status endpoint
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_status).

%% API
-export([init/2]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req0 = #{method := <<"GET">>}, Conn) ->
    Req = cowboy_req:reply(
        200,
        #{<<"content-type">> => "text/plain"},
        graphbase_apiserver_packet:encode({status, ok}),
        Req0
    ),
    {ok, Req, Conn};
        
init(Req0, Conn) ->
    Req = cowboy_req:reply(
        405,
        #{<<"allow">> => <<"GET">>},
        Req0
    ),
    {ok, Req, Conn}.
