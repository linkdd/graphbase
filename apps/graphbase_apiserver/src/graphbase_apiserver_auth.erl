%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver auth module
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_auth).

%% API
-export([
    authenticated/2
]).

%%====================================================================
%% API Functions
%%====================================================================

authenticated(Req, Function) ->
    handle_request(cowboy_req:header(<<"remote_user">>, Req), Req, Function).

%%====================================================================
%% Internal Functions
%%====================================================================

handle_request(undefined, Req, _Function) ->
    abort_401(unauthorized, Req);

handle_request(UserName, Req, Function) ->
    graphbase_backend_connection_pool:with(fun(Conn) ->
        MetaGraph = graphbase_metagraph:new(Conn),
        UserRequest = graphbase_acl_user:new(Conn, MetaGraph, UserName),
        case graphbase_entity_obj:fetch(UserRequest) of
            {ok, UserNode}  -> Function(UserNode, Req);
            {error, Reason} -> abort_401(Reason, Req)
        end
    end).

%%--------------------------------------------------------------------
abort_401(Reason, Req) ->  
    cowboy_req:reply(
        401,
        #{<<"content-type">> => <<"text/plain">>},
        graphbase_apiserver_packet:encode({error, Reason}),
        Req
    ).
