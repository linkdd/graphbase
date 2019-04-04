%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver user collection endpoint
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_user_collection).

%% API
-export([
    init_state/0,
    finalize_state/1,
    init/2
]).

%%====================================================================
%% API Functions
%%====================================================================

init_state() ->
    {ok, Conn} = graphbase_backend_connection_pool:acquire(),
    Conn.

finalize_state(Conn) ->
    graphbase_backend_connection_pool:release(Conn).

init(Req0 = #{method := <<"GET">>}, Conn) ->
    SystemGraph = graphbase_system_metagraph:new(Conn),
    Users = graphbase_system_metagraph:get_users(SystemGraph),
    Req = cowboy_req:reply(
        200,
        #{<<"content-type">> => "text/plain"},
        graphbase_apiserver_packet:encode({nodes, [
            #{
                id => graphbase_entity_obj:id(U),
                properties => graphbase_entity_obj:value(U)
            } || U <- Users]}),
        Req0
    ),
    {ok, Req, Conn};

init(Req0, Conn) ->
    Req = cowboy_req:reply(
        405,
        #{<<"allow">> => <<"GET,POST">>},
        Req0
    ),
    {ok, Req, Conn}.
