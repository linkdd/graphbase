%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver requests endpoint
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_requests).

%% API
-export([init/2]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req = #{method := <<"POST">>}, State) ->
    {ok, handle_post(cowboy_req:has_body(Req), Req), State};

init(Req0, State) ->
    Req = cowboy_req:reply(
        405,
        #{<<"allow">> => <<"POST">>},
        Req0
    ),
    {ok, Req, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_post(false, Req0) ->
    cowboy_req:reply(
        400,
        #{<<"content-type">> => "text/plain"},
        graphbase_apiserver_packet:encode({error, no_body}),
        Req0
    );

handle_post(true, Req) ->
    graphbase_apiserver_auth:authenticated(Req, fun(User, Req0) ->
        {ok, RawRequest, Req1} = cowboy_req:read_body(Req0),
        Request = graphbase_apiserver_packet:decode(RawRequest),
        Reply = graphbase_core:with(
            fun() -> graphbase_dsl_api:start_link(User) end,
            fun(API) -> graphbase_dsl_api:stop(API) end,
            fun(API) -> graphbase_dsl_api:interpret(API, Request) end
        ),
        cowboy_req:reply(
            200,
            #{<<"content-type">> => <<"text/plain">>},
            graphbase_apiserver_packet:encode(Reply),
            Req1
        )
    end).
