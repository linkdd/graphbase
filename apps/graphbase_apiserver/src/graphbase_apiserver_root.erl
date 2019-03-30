%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver root endpoint
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_root).

%% API
-export([init/2]).

%%====================================================================
%% API
%%====================================================================

init(Req0 = #{method := <<"GET">>}, State) ->
    Req = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/plain">>},
        graphbase_apiserver_packet:encode([
            {link, "/", [
                {description, "API reference"},
                {allow, ["GET"]}
            ]},
            {link, "/status", [
                {description, "Current cluster health"},
                {allow, ["GET"]}
            ]},
            {link, "/api/requests", [
                {description, "Request management"},
                {endpoints, [
                    {link, "/", [
                        {description, "Request handler"},
                        {allow, ["POST"]}
                    ]}
                ]}
            ]}
        ]),
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
