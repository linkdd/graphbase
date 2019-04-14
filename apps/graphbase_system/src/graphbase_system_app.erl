%%%-------------------------------------------------------------------
%% @doc graphbase_system public API
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_system_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    join_cluster(),
    initialize(),
    graphbase_system_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

join_cluster() ->
    case graphbase_core_utils:get_confopt(cluster_cookie) of
        undefined -> ok;
        Cookie    -> erlang:set_cookie(node(), Cookie)
    end,
    case graphbase_core_utils:get_confopt(cluster_gateway) of
        undefined      -> ok;
        ClusterGateway ->
            case net_kernel:connect_node(ClusterGateway) of
                true  -> ok;
                false -> {error, {unable_to_join_cluster, ClusterGateway}}
            end
    end.

%%--------------------------------------------------------------------
initialize() ->
    graphbase_backend_connection_pool:with(
        fun(Conn) ->
            MetaGraphRef = create_meta_graph(Conn),
            User = create_admin_user(Conn, MetaGraphRef),
            ok = create_admin_acl(Conn, MetaGraphRef, User, read),
            ok = create_admin_acl(Conn, MetaGraphRef, User, write)
        end
    ).

%%--------------------------------------------------------------------
create_meta_graph(Conn) ->
    MetaGraph0 = graphbase_metagraph:new(Conn),
    MetaGraphRef0 = graphbase_metagraph_graph:new(Conn, <<"system">>, MetaGraph0),
    MetaGraph1 = graphbase_metagraph:add_graph(MetaGraph0, MetaGraphRef0),
    {ok, _} = graphbase_entity_obj:save(MetaGraph1),
    {ok, MetaGraphRef1} = graphbase_entity_obj:save(MetaGraphRef0),
    MetaGraphRef1.

%%--------------------------------------------------------------------
create_admin_user(Conn, MetaGraphRef) ->
    {ok, MetaGraph0} = graphbase_metagraph_graph:entity(MetaGraphRef),
    User0 = graphbase_acl_user:new(Conn, MetaGraph0, <<"admin">>),
    MetaGraph1 = graphbase_metagraph:add_user(MetaGraph0, User0),
    {ok, _} = graphbase_entity_obj:save(MetaGraph1),
    {ok, User1} = graphbase_entity_obj:save(User0),
    User1.

%%--------------------------------------------------------------------
create_admin_acl(Conn, MetaGraphRef, User, Access) ->
    {ok, MetaGraph0} = graphbase_metagraph_graph:entity(MetaGraphRef),
    ACL0 = graphbase_acl:new(Conn, MetaGraph0, User, Access),
    ACL1 = graphbase_acl:grant(ACL0, MetaGraphRef),
    MetaGraph1 = graphbase_metagraph:add_acl(MetaGraph0, ACL1),
    {ok, _} = graphbase_entity_obj:save(MetaGraph1),
    {ok, _} = graphbase_entity_obj:save(ACL1),
    ok.
