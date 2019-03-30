-module(graphbase_system_graph).

%% API
-export([
    set_user/2,
    get_user/1,
    del_user/1,
    grant_read_access/2,
    grant_write_access/2,
    revoke_read_access/2,
    revoke_write_access/2,
    clear_read_access/1,
    clear_write_access/1,
    has_read_access/2,
    has_write_access/2
]).

-include_lib("graphbase_entity/include/entity.hrl").

%%====================================================================
%% API functions
%%====================================================================

set_user(Name, Credentials) ->
    SystemGraph = get_or_create(),
    UserNodeReq = graphbase_entity_node:new(<<"user_", Name/binary>>, SystemGraph),
    {ok, UserNode0} = graphbase_entity_api:fetch(UserNodeReq),
    UserNode1 = graphbase_entity_obj:update(
        {<<"credentials">>, register},
        fun(R) -> riakc_register:set(Credentials, R) end,
        UserNode0
    ),
    graphbase_entity_api:save(UserNode1).

%%--------------------------------------------------------------------
get_user(Name) ->
    SystemGraph = get_or_create(),
    UserNodeReq = graphbase_entity_node:new(<<"user_", Name/binary>>, SystemGraph),
    {ok, UserNode} = graphbase_entity_api:fetch(UserNodeReq),
    UserNode.

%%--------------------------------------------------------------------
del_user(Name) ->
    User = get_user(Name),
    UserRef = graphbase_entity_api:ref(User),
    clear_read_access(UserRef),
    clear_write_access(UserRef),
    graphbase_entity_api:delete(User).

%%--------------------------------------------------------------------
grant_read_access(GraphRef, UserRef) ->
    grant_access("read", GraphRef, UserRef).

%%--------------------------------------------------------------------
grant_write_access(GraphRef, UserRef) ->
    grant_access("write", GraphRef, UserRef).

%%--------------------------------------------------------------------
revoke_read_access(GraphRef, UserRef) ->
    revoke_access("read", GraphRef, UserRef).

%%--------------------------------------------------------------------
revoke_write_access(GraphRef, UserRef) ->
    revoke_access("write", GraphRef, UserRef).

%%--------------------------------------------------------------------
clear_read_access(UserRef) ->
    clear_access("read", UserRef).

%%--------------------------------------------------------------------
clear_write_access(UserRef) ->
    clear_access("write", UserRef).

%%--------------------------------------------------------------------
has_read_access(GraphRef, UserRef) ->
    has_access("read", GraphRef, UserRef).
    
%%--------------------------------------------------------------------
has_write_access(GraphRef, UserRef) ->
    has_access("write", GraphRef, UserRef).

%%====================================================================
%% Internal functions
%%====================================================================

get_or_create() ->
    SystemGraph = graphbase_entity_graph:new(<<"system">>),
    graphbase_entity_api:save(SystemGraph).

%%--------------------------------------------------------------------
grant_access(Access, GraphRef, UserRef) ->
    KeyPreffix = <<"acl_", Access, "_">>,
    SystemGraph = get_or_create(),
    {ok, User} = graphbase_entity_api:unref(UserRef),
    UserId = graphbase_entity_obj:get_id(User),
    Edge0 = graphbase_entity_edge:new(<<KeyPreffix/binary, UserId/binary>>, SystemGraph, UserRef),
    Edge1 = graphbase_entity_obj:update(
        {<<"kind">>, register},
        fun(R) -> riakc_register:set(<<"can_read">>, R) end,
        Edge0
    ),
    Edge2 = graphbase_entity_edge:add_neighbors(Edge1, [GraphRef]),
    graphbase_entity_api:save(Edge2).

%%--------------------------------------------------------------------
revoke_access(Access, GraphRef, UserRef) ->
    KeyPreffix = <<"acl_", Access, "_">>,
    SystemGraph = get_or_create(),
    {ok, User} = graphbase_entity_api:unref(UserRef),
    UserId = graphbase_entity_obj:get_id(User),
    Edge0 = graphbase_entity_edge:new(<<KeyPreffix/binary, UserId/binary>>, SystemGraph, UserRef),
    Edge1 = graphbase_entity_obj:update(
        {<<"kind">>, register},
        fun(R) -> riakc_register:set(<<"can_read">>, R) end,
        Edge0
    ),
    Edge2 = graphbase_entity_edge:del_neighbors(Edge1, [GraphRef]),
    graphbase_entity_api:save(Edge2).

%%--------------------------------------------------------------------
clear_access(Access, UserRef) ->
    KeyPreffix = <<"acl_", Access, "_">>,
    SystemGraph = get_or_create(),
    {ok, User} = graphbase_entity_api:unref(UserRef),
    UserId = graphbase_entity_obj:get_id(User),
    Edge = graphbase_entity_edge:new(<<KeyPreffix/binary, UserId/binary>>, SystemGraph, UserRef),
    graphbase_entity_api:delete(Edge).

%%--------------------------------------------------------------------
has_access(Access, GraphRef, UserRef) ->
    KeyPreffix = <<"acl_", Access, "_">>,
    SystemGraph = get_or_create(),
    {ok, User} = graphbase_entity_api:unref(UserRef),
    UserId = graphbase_entity_obj:get_id(User),
    EdgeReq = graphbase_entity_edge:new(<<KeyPreffix/binary, UserId/binary>>, SystemGraph, UserRef),
    {ok, Edge} = graphbase_entity_api:fetch(EdgeReq),
    EdgeValue = graphbase_entity_obj:value(Edge),
    Neighbors = proplists:get_value({<<"neighbors">>, set}, EdgeValue, []),
    lists:member(GraphRef, Neighbors).
