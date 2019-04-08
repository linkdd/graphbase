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

init(Req0 = #{method := <<"GET">>}, State) ->
    Req = cowboy_req:reply(
        200,
        #{<<"content-type">> => "text/plain"},
        graphbase_apiserver_packet:encode({status, [
            {nodes, [node() | nodes()]}
        ]}),
        Req0
    ),
    {ok, Req, State};
        
init(Req0, State) ->
    Req = cowboy_req:reply(
        405,
        #{<<"allow">> => <<"GET">>},
        Req0
    ),
    {ok, Req, State}.
