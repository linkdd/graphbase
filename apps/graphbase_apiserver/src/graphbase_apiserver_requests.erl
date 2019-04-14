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
        {ok, Request, Req1} = cowboy_req:read_body(Req0),
        {Status, Reply} = try
            graphbase_dsl:interpret(User, Request)
        of
            R -> {200, R}
        catch
            Exc:Reason:Stack -> {500, {error, {Exc, Reason, Stack}}}
        end,
        cowboy_req:reply(
            Status,
            #{<<"content-type">> => <<"text/plain">>},
            graphbase_apiserver_packet:encode(Reply),
            Req1
        )
    end).
