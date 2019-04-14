-module(graphbase_core_api).

%% API exports
-export([
    value/2,
    graph/2,
    nodes/2,
    edges/2,
    walk/2,
    filter/2
]).

%%====================================================================
%% API functions
%%====================================================================

value(_User, Arguments) ->
    Entities = get_argument(entities, Arguments, []),
    EntitySet = case is_list(Entities) of
        true  -> Entities;
        false -> [Entities]
    end,
    graphbase_backend_connection_pool:with(fun(Conn) ->
        F = fun(Ref) ->
            case graphbase_entity_obj:fetch(graphbase_entity_obj:unref(Conn, Ref)) of
                {ok, Entity} -> graphbase_entity_obj:value(Entity);
                Error        -> throw(Error)
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
    case get_argument(id, Arguments) of
        undefined -> {error, {missing_argument, id}};
        GraphId   ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                MetaGraph = graphbase_system_metagraph:new(Conn),
                RequestedGraph = graphbase_system_graph:new(Conn, GraphId, MetaGraph),
                {ok, GraphEntity} = graphbase_system_graph:entity(RequestedGraph),

                case can_access(Conn, User, GraphEntity, read) of
                    true  -> {ok, graphbase_entity_obj:ref(GraphEntity)};
                    false -> {error, {unauthorized, read}}
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
filter(_User, Arguments) ->
    graphbase_backend_connection_pool:with(fun(Conn) ->
        Entities = get_argument(entities, Arguments, []),
        Rules = get_argument(rules, Arguments, []),
        {ok, P} = emapred_pipeline:new(
            fun(Ref) ->
                Entity = graphbase_entity_obj:unref(Conn, Ref),
                case graphbase_core_filter:match(Rules, graphbase_entity_obj:value(Entity)) of
                    true  -> {emit, {entity, Ref}};
                    false -> ok
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
    MetaGraph = graphbase_system_metagraph:new(Conn),
    ACL = graphbase_system_acl:new(Conn, MetaGraph, User, Access),
    GraphId = graphbase_entity_obj:id(Graph),
    GraphRef = graphbase_system_graph:new(Conn, GraphId, MetaGraph),
    graphbase_system_acl:has(ACL, GraphRef).

%%--------------------------------------------------------------------
get_argument(Name, Arguments) ->
    get_argument(Name, Arguments, undefined).

%%--------------------------------------------------------------------
get_argument(Name, Arguments, Default) ->
    proplists:get_value(list_to_binary(atom_to_list(Name)), Arguments, Default).

