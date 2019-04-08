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
    initialize(),
    graphbase_system_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

initialize() ->
    {ok, Conn} = graphbase_backend_connection_pool:acquire(),
    MetaGraphRef = create_meta_graph(Conn),
    User = create_admin_user(Conn, MetaGraphRef),
    ok = create_admin_acl(Conn, MetaGraphRef, User, read),
    ok = create_admin_acl(Conn, MetaGraphRef, User, write),
    graphbase_backend_connection_pool:release(Conn).

%%--------------------------------------------------------------------
create_meta_graph(Conn) ->
    MetaGraph0 = graphbase_system_metagraph:new(Conn),
    MetaGraphRef0 = graphbase_system_graph:new(Conn, <<"system">>, MetaGraph0),
    MetaGraph1 = graphbase_system_metagraph:add_graph(MetaGraph0, MetaGraphRef0),
    {ok, _} = graphbase_entity_obj:save(MetaGraph1),
    {ok, MetaGraphRef1} = graphbase_entity_obj:save(MetaGraphRef0),
    MetaGraphRef1.

%%--------------------------------------------------------------------
create_admin_user(Conn, MetaGraphRef) ->
    {ok, MetaGraph0} = graphbase_system_graph:entity(MetaGraphRef),
    User0 = graphbase_system_user:new(Conn, MetaGraph0, <<"admin">>),
    MetaGraph1 = graphbase_system_metagraph:add_user(MetaGraph0, User0),
    {ok, _} = graphbase_entity_obj:save(MetaGraph1),
    {ok, User1} = graphbase_entity_obj:save(User0),
    User1.

%%--------------------------------------------------------------------
create_admin_acl(Conn, MetaGraphRef, User, Access) ->
    {ok, MetaGraph0} = graphbase_system_graph:entity(MetaGraphRef),
    ACL0 = graphbase_system_acl:new(Conn, MetaGraph0, User, Access),
    ACL1 = graphbase_system_acl:grant(ACL0, MetaGraphRef),
    MetaGraph1 = graphbase_system_metagraph:add_acl(MetaGraph0, ACL1),
    {ok, _} = graphbase_entity_obj:save(MetaGraph1),
    {ok, _} = graphbase_entity_obj:save(ACL1),
    ok.
