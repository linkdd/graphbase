-module(graphbase_core_api).

%% API exports
-export([
    value/2,
    graph/2,
    nodes/2,
    edges/2,
    filter/2
]).

%%====================================================================
%% API functions
%%====================================================================

value(_User, Arguments) ->
    Entities = proplists:get_value(entities, Arguments, []),
    EntitySet = case is_list(Entities) of
        true  -> Entities;
        false -> [Entities]
    end,
    graphbase_backend_connection_pool:with(fun(Conn) ->
        F = fun(Ref) ->
            case graphbase_entity_obj:fetch(graphbase_entity_obj:unref(Conn, Ref)) of
                {ok, Entity} ->
                    Id = graphbase_entity_obj:id(Entity),
                    Value = graphbase_entity_obj:value(Entity),
                    [{{<<"$id">>, register}, Id} | Value];

                Error ->
                    throw(Error)
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
            _:Reason -> {error, {unable_to_get_value, Reason}}
        end
    end).

%%--------------------------------------------------------------------
graph(User, Arguments) ->
    case proplists:get_value(id, Arguments) of
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
    case proplists:get_value(graph, Arguments) of
        undefined -> {error, {missing_argument, graph}};
        GraphRef  ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                Graph = graphbase_entity_obj:unref(Conn, GraphRef),
                Rules = proplists:get_value(rules, Arguments, []),

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
    case proplists:get_value(graph, Arguments) of
        undefined -> {error, {missing_argument, graph}};
        GraphRef  ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                Graph = graphbase_entity_obj:unref(Conn, GraphRef),
                Rules = proplists:get_value(rules, Arguments, []),

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

filter(_User, Arguments) ->
    graphbase_backend_connection_pool:with(fun(Conn) ->
        Entities = proplists:get_value(entities, Arguments, []),
        Rules = proplists:get_value(rules, Arguments, []),
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
