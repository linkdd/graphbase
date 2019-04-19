-module(graphbase_dsl_api).

%% API exports
-export([
    value/2,
    graph/2,
    node/2,
    nodes/2,
    edge/2,
    edges/2,
    walk/2,
    connect/2,
    disconnect/2,
    match/2,
    filter/2
]).

%%====================================================================
%% API functions
%%====================================================================

value(User, Arguments) ->
    Entities = get_argument(entities, Arguments, []),
    EntitySet = case is_list(Entities) of
        true  -> Entities;
        false -> [Entities]
    end,
    graphbase_backend_connection_pool:with(fun(Conn) ->
        F = fun(Ref) ->
            Entity = graphbase_entity_obj:unref(Conn, Ref),
            Access = case graphbase_entity_scope:graph(Entity) of
                undefined -> true;
                GraphRef  ->
                    Graph = graphbase_entity_obj:unref(Conn, GraphRef),
                    can_access(Conn, User, Graph, read)
            end,
            case Access of
                true  -> graphbase_entity_obj:value(Entity);
                false -> {error, {unauthorized, read}}
            end
        end,
        try
            [F(Ref) || Ref <- EntitySet]
        of
            Result ->
                case is_list(Entities) of
                    true  -> {ok, Result};
                    false -> {ok, lists:nth(1, Result)}
                end
        catch
            _:Reason:Stack -> {error, {unable_to_get_value, Reason, Stack}}
        end
    end).

%%--------------------------------------------------------------------
graph(User, Arguments) ->
    graphbase_backend_connection_pool:with(fun(Conn) ->
        MetaGraph = graphbase_metagraph:new(Conn),
        RequestedGraph = case get_argument(id, Arguments) of
            undefined -> graphbase_metagraph_graph:new(Conn, MetaGraph);
            GraphId   -> graphbase_metagraph_graph:new(Conn, GraphId, MetaGraph)
        end,
        {ok, GraphEntity} = graphbase_metagraph_graph:entity(RequestedGraph),

        case can_access(Conn, User, GraphEntity, read) of
            true  -> {ok, graphbase_entity_obj:ref(GraphEntity)};
            false -> {error, {unauthorized, read}}
        end
    end).

%%--------------------------------------------------------------------
node(User, Arguments) ->
    case get_argument(graph, Arguments) of
        undefined -> {error, {missing_argument, graph}};
        GraphRef  ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                Graph0 = graphbase_entity_obj:unref(Conn, GraphRef),

                case can_access(Conn, User, Graph0, write) of
                    true ->
                        Node0 = case get_argument(id, Arguments) of
                            undefined -> graphbase_entity_node:new(Conn, Graph0);
                            Id        -> graphbase_entity_node:new(Conn, Id, Graph0)
                        end,
                        Node1 = graphbase_entity_node:set_kind(Node0, get_argument(kind, Arguments, <<"">>)),
                        {ok, Node2} = graphbase_entity_obj:save(Node1),
                        Graph1 = graphbase_entity_graph:add_nodes(Graph0, [Node2]),
                        {ok, _} = graphbase_entity_obj:save(Graph1),
                        {ok, graphbase_entity_obj:ref(Node2)};
                    
                    false ->
                        {error, {unauthorized, write}}
                end
            end)
    end.

%%--------------------------------------------------------------------
nodes(User, Arguments) ->
    case get_argument(graph, Arguments) of
        undefined -> {error, {missing_argument, graph}};
        GraphRef  ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                Graph = graphbase_entity_obj:unref(Conn, GraphRef),
                Rules = get_argument(rules, Arguments, []),

                case can_access(Conn, User, Graph, read) of
                    true ->
                        {ok, [
                            graphbase_entity_obj:ref(Node) ||
                            Node <- graphbase_entity_graph:filter_nodes(Graph, Rules)
                        ]};

                    false ->
                        {error, {unauthorized, read}}
                end
            end)
    end.

%%--------------------------------------------------------------------
edge(User, Arguments) ->
    case get_argument(graph, Arguments) of
        undefined -> {error, {missing_argument, graph}};
        GraphRef  ->
            case get_argument(node, Arguments) of
                undefined -> {error, {missing_argument, node}};
                NodeRef   ->
                    graphbase_backend_connection_pool:with(fun(Conn) ->
                        Graph0 = graphbase_entity_obj:unref(Conn, GraphRef),

                        case can_access(Conn, User, Graph0, write) of
                            true ->
                                Node = graphbase_entity_obj:unref(Conn, NodeRef),
                                Edge0 = case get_argument(id, Arguments) of
                                    undefined -> graphbase_entity_edge:new(Conn, Graph0, Node);
                                    Id        -> graphbase_entity_node:new(Conn, Id, Graph0, Node)
                                end,
                                RelationShip = get_argument(relationship, Arguments, <<"">>),
                                Edge1 = graphbase_entity_edge:set_relationship(Edge0, RelationShip),
                                {ok, Edge2} = graphbase_entity_obj:save(Edge1),
                                Graph1 = graphbase_entity_graph:add_edges(Graph0, [Edge2]),
                                {ok, _} = graphbase_entity_obj:save(Graph1),
                                {ok, graphbase_entity_obj:ref(Edge2)};
                            
                            false ->
                                {error, {unauthorized, write}}
                        end
                    end)
            end
    end.

%%--------------------------------------------------------------------
edges(User, Arguments) ->
    case get_argument(graph, Arguments) of
        undefined -> {error, {missing_argument, graph}};
        GraphRef  ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                Graph = graphbase_entity_obj:unref(Conn, GraphRef),
                Rules = get_argument(rules, Arguments, []),

                case can_access(Conn, User, Graph, read) of
                    true ->
                        {ok, [
                            graphbase_entity_obj:ref(Node) ||
                            Node <- graphbase_entity_graph:filter_edges(Graph, Rules)
                        ]};

                    false ->
                        {error, {unauthorized, read}}
                end
            end)
    end.

%%--------------------------------------------------------------------
walk(User, Arguments) ->
    case get_argument(graph, Arguments) of
        undefined -> {error, {missing_argument, graph}};
        GraphRef  ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                Graph = graphbase_entity_obj:unref(Conn, GraphRef),
                From = get_argument(from, Arguments, []),
                To = get_argument(to, Arguments, []),
                Via = get_argument(via, Arguments, []),

                case can_access(Conn, User, Graph, read) of
                    true ->
                        Nodes = graphbase_entity_graph:filter_nodes(Graph, From),
                        Relationships = graphbase_entity_graph:filter_edges(Graph, {all_of, [Via, [
                            {property, {<<"node">>, register}, {in, [
                                graphbase_entity_obj:ref(Node) || Node <- Nodes
                            ]}}
                        ]]}),

                        {ok, P} = emapred_pipeline:new(
                            fun(Edge) ->
                                {emit, {neighbors, [
                                    graphbase_entity_obj:id(Neighbor) ||
                                    Neighbor <- graphbase_entity_edge:get_neighbors(Edge)
                                ]}}
                            end,
                            fun(neighbors, EdgeNeighbors, AllNeighbors) ->
                                {ok, sets:union(sets:from_list(EdgeNeighbors), AllNeighbors)}
                            end,
                            sets:new()
                        ),
                        lists:foreach(
                            fun(Edge) ->
                                ok = emapred_pipeline:send(P, Edge),
                                ok
                            end,
                            Relationships
                        ),
                        AllNeighbors = emapred_pipeline:stop(P),

                        Targets = graphbase_entity_graph:filter_nodes(Graph, {all_of, [To, [
                            {property, {<<"$id">>, register}, {in, sets:to_list(AllNeighbors)}}
                        ]]}),

                        {ok, [graphbase_entity_obj:ref(Target) || Target <- Targets]};

                    false ->
                        {error, {unauthorized, read}}
                end
            end)
    end.

%%--------------------------------------------------------------------
connect(User, Arguments) ->
    case get_argument(nodes, Arguments) of
        undefined -> {error, {missing_argument, node}};
        NodeRefs  ->
            case get_argument(edge, Arguments) of
                undefined -> {error, {missing_argument, edge}};
                EdgeRef   ->
                    graphbase_backend_connection_pool:with(fun(Conn) ->
                        Nodes = [graphbase_entity_obj:unref(Conn, NodeRef) || NodeRef <- NodeRefs],
                        Edge0 = graphbase_entity_obj:unref(Conn, EdgeRef),
                        Graph = graphbase_entity_obj:unref(Conn, graphbase_entity_scope:graph(Edge0)),

                        case can_access(Conn, User, Graph, write) of
                            true ->
                                Edge1 = graphbase_entity_edge:add_neighbors(Edge0, Nodes),
                                {ok, Edge2} = graphbase_entity_obj:save(Edge1),
                                {ok, [Edge2]};
                            
                            false ->
                                {error, {unauthorized, write}}
                        end
                    end)
            end
    end.

%%--------------------------------------------------------------------
disconnect(User, Arguments) ->
    case get_argument(nodes, Arguments) of
        undefined -> {error, {missing_argument, node}};
        NodeRefs  ->
            case get_argument(edge, Arguments) of
                undefined -> {error, {missing_argument, edge}};
                EdgeRef   ->
                    graphbase_backend_connection_pool:with(fun(Conn) ->
                        Nodes = [graphbase_entity_obj:unref(Conn, NodeRef) || NodeRef <- NodeRefs],
                        Edge0 = graphbase_entity_obj:unref(Conn, EdgeRef),
                        Graph = graphbase_entity_obj:unref(Conn, graphbase_entity_scope:graph(Edge0)),

                        case can_access(Conn, User, Graph, write) of
                            true ->
                                Edge1 = graphbase_entity_edge:del_neighbors(Edge0, Nodes),
                                {ok, Edge2} = graphbase_entity_obj:save(Edge1),
                                {ok, [Edge2]};
                            
                            false ->
                                {error, {unauthorized, write}}
                        end
                    end)
            end
    end.

%%--------------------------------------------------------------------
match(User, Arguments) ->
    Rules = get_argument(rules, Arguments, []),
    case get_argument(entity, Arguments) of
        undefined -> {error, {missing_argument, entity}};
        EntityRef ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                Entity = graphbase_entity_obj:unref(Conn, EntityRef),
                Graph = graphbase_entity_obj:unref(Conn, graphbase_entity_scope:graph(Entity)),

                case can_access(Conn, User, Graph, read) of
                    true  -> graphbase_core_filter:match(Rules, graphbase_entity_obj:value(Entity));
                    false -> {error, {unauthorized, read}}
                end
            end)
    end.

%%--------------------------------------------------------------------
filter(User, Arguments) ->
    graphbase_backend_connection_pool:with(fun(Conn) ->
        Entities = get_argument(entities, Arguments, []),
        Rules = get_argument(rules, Arguments, []),
        {ok, P} = emapred_pipeline:new(
            fun(Ref) ->
                Entity = graphbase_entity_obj:unref(Conn, Ref),
                Graph = graphbase_entity_obj:unref(Conn, graphbase_entity_scope:graph(Entity)),

                case can_access(Conn, User, Graph, read) of
                    true ->
                        case graphbase_core_filter:match(Rules, graphbase_entity_obj:value(Entity)) of
                            true  -> {emit, {entity, Ref}};
                            false -> ok
                        end;
                    
                    false ->
                        {error, {unauthorized, read}}
                end
            end,
            fun(entity, Ref, Result) ->
                {ok, [Ref | Result]}
            end,
            []
        ),
        lists:foreach(
            fun(Entity) ->
                ok = emapred_pipeline:send(P, Entity),
                ok
            end,
            Entities
        ),
        {ok, emapred_pipeline:stop(P)}
    end).

%%====================================================================
%% Internal functions
%%====================================================================

can_access(Conn, User, Graph, Access) ->
    MetaGraph = graphbase_metagraph:new(Conn),
    ACL = graphbase_acl:new(Conn, MetaGraph, User, Access),
    GraphId = graphbase_entity_obj:id(Graph),
    GraphRef = graphbase_metagraph_graph:new(Conn, GraphId, MetaGraph),
    graphbase_acl:has(ACL, GraphRef).

%%--------------------------------------------------------------------
get_argument(Name, Arguments) ->
    get_argument(Name, Arguments, undefined).

%%--------------------------------------------------------------------
get_argument(Name, Arguments, Default) ->
    proplists:get_value(list_to_binary(atom_to_list(Name)), Arguments, Default).

