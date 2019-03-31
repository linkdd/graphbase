-module(graphbase_system_acl_impl).

%% API
-export([
    list_users/1,
    set_user/3,
    get_user/2,
    del_user/2,
    grant_read_access/3,
    grant_write_access/3,
    revoke_read_access/3,
    revoke_write_access/3,
    clear_read_access/2,
    clear_write_access/2,
    has_read_access/3,
    has_write_access/3
]).

%%====================================================================
%% API functions
%%====================================================================

list_users(Conn) ->
    SystemGraph = graphbase_system_metagraph:get_or_create(Conn),
    GraphValue = graphbase_entity_obj:value(SystemGraph),
    {ok, Pipeline} = emapred_pipeline:new(
        fun(NodeRef) ->
            {ok, Node} = graphbase_entity_api:fetch_ref(Conn, NodeRef),
            NodeValue = graphbase_entity_obj:value(Node),
            case proplists:get_value({<<"kind">>, set}, NodeValue) of
                <<"user">> -> {emit, {user, Node}};
                _          -> ok
            end
        end,
        fun(user, Node, Result) ->
            {ok, [Node | Result]}
        end,
        []
    ),
    lists:foreach(
        fun(NodeRef) ->
            ok = emapred_pipeline:send(Pipeline, NodeRef),
            ok
        end,
        proplists:get_value({<<"nodes">>, set}, GraphValue, [])
    ),
    {ok, emapred_pipeline:stop(Pipeline)}.

%%--------------------------------------------------------------------
set_user(Conn, Name, Credentials) ->
    SystemGraph0 = graphbase_system_metagraph:get_or_create(Conn),
    UserNodeReq = graphbase_entity_node:new(<<"user_", Name/binary>>, SystemGraph0),
    {ok, UserNode0} = graphbase_entity_api:fetch(Conn, UserNodeReq),
    UserNode1 = graphbase_entity_obj:update(
        {<<"kind">>, register},
        fun(R) -> riakc_register:set(<<"user">>, R) end,
        graphbase_entity_obj:update(
            {<<"credentials">>, register},
            fun(R) -> riakc_register:set(Credentials, R) end,
            UserNode0
        )
    ),
    {ok, UserNode} = graphbase_entity_api:save(Conn, UserNode1),
    SystemGraph1 = graphbase_entity_graph:add_nodes(SystemGraph0, [UserNode]),
    {ok, _} = graphbase_entity_api:save(Conn, SystemGraph1),
    {ok, UserNode}.


%%--------------------------------------------------------------------
get_user(Conn, Name) ->
    SystemGraph = graphbase_system_metagraph:get_or_create(Conn),
    UserNodeReq = graphbase_entity_node:new(<<"user_", Name/binary>>, SystemGraph),
    {ok, UserNode} = graphbase_entity_api:fetch(Conn, UserNodeReq),
    {ok, UserNode}.

%%--------------------------------------------------------------------
del_user(Conn, Name) ->
    SystemGraph0 = graphbase_system_metagraph:get_or_create(Conn),
    UserNodeReq = graphbase_entity_node:new(<<"user_", Name/binary>>, SystemGraph0),
    {ok, UserNode} = graphbase_entity_api:fetch(Conn, UserNodeReq),
    UserRef = graphbase_entity_obj:ref(UserNode),
    clear_read_access(Conn, UserRef),
    clear_write_access(Conn, UserRef),
    SystemGraph1 = graphbase_entity_graph:del_nodes(SystemGraph0, [UserNode]),
    {ok, _} = graphbase_entity_api:save(Conn, SystemGraph1),
    graphbase_entity_api:delete(Conn, UserNode).

%%--------------------------------------------------------------------
grant_read_access(Conn, GraphRef, UserRef) ->
    grant_access(Conn, "read", GraphRef, UserRef).

%%--------------------------------------------------------------------
grant_write_access(Conn, GraphRef, UserRef) ->
    grant_access(Conn, "write", GraphRef, UserRef).

%%--------------------------------------------------------------------
revoke_read_access(Conn, GraphRef, UserRef) ->
    revoke_access(Conn, "read", GraphRef, UserRef).

%%--------------------------------------------------------------------
revoke_write_access(Conn, GraphRef, UserRef) ->
    revoke_access(Conn, "write", GraphRef, UserRef).

%%--------------------------------------------------------------------
clear_read_access(Conn, UserRef) ->
    clear_access(Conn, "read", UserRef).

%%--------------------------------------------------------------------
clear_write_access(Conn, UserRef) ->
    clear_access(Conn, "write", UserRef).

%%--------------------------------------------------------------------
has_read_access(Conn, GraphRef, UserRef) ->
    has_access(Conn, "read", GraphRef, UserRef).
    
%%--------------------------------------------------------------------
has_write_access(Conn, GraphRef, UserRef) ->
    has_access(Conn, "write", GraphRef, UserRef).

%%====================================================================
%% Internal functions
%%====================================================================

grant_access(Conn, Access, GraphRef, UserRef) ->
    KeyPreffix = <<"acl_", Access, "_">>,
    SystemGraph0 = graphbase_system_metagraph:get_or_create(Conn),
    {ok, User} = graphbase_entity_api:fetch_ref(Conn, UserRef),
    UserId = graphbase_entity_obj:id(User),
    Edge0 = graphbase_entity_edge:new(<<KeyPreffix/binary, UserId/binary>>, SystemGraph0, UserRef),
    Edge1 = graphbase_entity_obj:update(
        {<<"kind">>, register},
        fun(R) -> riakc_register:set(<<"can_read">>, R) end,
        Edge0
    ),
    Edge2 = graphbase_entity_edge:add_neighbors(Edge1, [GraphRef]),
    {ok, Edge} = graphbase_entity_api:save(Conn, Edge2),
    SystemGraph1 = graphbase_entity_graph:add_edges(SystemGraph0, [Edge]),
    {ok, _} = graphbase_entity_api:save(Conn, SystemGraph1),
    {ok, Edge}.

%%--------------------------------------------------------------------
revoke_access(Conn, Access, GraphRef, UserRef) ->
    KeyPreffix = <<"acl_", Access, "_">>,
    SystemGraph0 = graphbase_system_metagraph:get_or_create(Conn),
    {ok, User} = graphbase_entity_api:fetch_ref(Conn, UserRef),
    UserId = graphbase_entity_obj:id(User),
    Edge0 = graphbase_entity_edge:new(<<KeyPreffix/binary, UserId/binary>>, SystemGraph0, UserRef),
    Edge1 = graphbase_entity_obj:update(
        {<<"kind">>, register},
        fun(R) -> riakc_register:set(<<"can_read">>, R) end,
        Edge0
    ),
    Edge2 = graphbase_entity_edge:del_neighbors(Edge1, [GraphRef]),
    {ok, Edge} = graphbase_entity_api:save(Conn, Edge2),
    SystemGraph1 = graphbase_entity_graph:add_edges(SystemGraph0, [Edge]),
    {ok, _} = graphbase_entity_api:save(Conn, SystemGraph1),
    {ok, Edge}.

%%--------------------------------------------------------------------
clear_access(Conn, Access, UserRef) ->
    KeyPreffix = <<"acl_", Access, "_">>,
    SystemGraph = graphbase_system_metagraph:get_or_create(Conn),
    {ok, User} = graphbase_entity_api:fetch_ref(Conn, UserRef),
    UserId = graphbase_entity_obj:id(User),
    Edge = graphbase_entity_edge:new(<<KeyPreffix/binary, UserId/binary>>, SystemGraph, UserRef),
    {ok, _} = graphbase_entity_graph:del_edges(SystemGraph, [Edge]),
    ok = graphbase_entity_api:delete(Conn, Edge),
    ok.

%%--------------------------------------------------------------------
has_access(Conn, Access, GraphRef, UserRef) ->
    KeyPreffix = <<"acl_", Access, "_">>,
    SystemGraph = graphbase_system_metagraph:get_or_create(Conn),
    {ok, User} = graphbase_entity_api:fetch_ref(Conn, UserRef),
    UserId = graphbase_entity_obj:id(User),
    EdgeReq = graphbase_entity_edge:new(<<KeyPreffix/binary, UserId/binary>>, SystemGraph, UserRef),
    {ok, Edge} = graphbase_entity_api:fetch(Conn, EdgeReq),
    EdgeValue = graphbase_entity_obj:value(Edge),
    Neighbors = proplists:get_value({<<"neighbors">>, set}, EdgeValue, []),
    lists:member(GraphRef, Neighbors).
