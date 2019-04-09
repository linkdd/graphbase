-module(graphbase_core_api).

%% API exports
-export([
    value/2,
    graph/2,
    nodes/2,
    edges/2
]).

%%====================================================================
%% API functions
%%====================================================================

value(_User, Arguments) ->
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
            [F(Ref) || Ref <- proplists:get_value(entities, Arguments, [])]
        of
            Result -> {ok, Result}
        catch
            _:Reason -> {error, {unable_to_get_value, Reason}}
        end
    end).

%%--------------------------------------------------------------------
graph(_User, Arguments) ->
    case proplists:get_value(id, Arguments) of
        undefined -> {error, {missing_argument, id}};
        GraphId   ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                MetaGraph = graphbase_system_metagraph:new(Conn),
                RequestedGraph = graphbase_system_graph:new(Conn, GraphId, MetaGraph),
                {ok, GraphEntity} = graphbase_system_graph:entity(RequestedGraph),
                {ok, graphbase_entity_obj:ref(GraphEntity)}
            end)
    end.

%%--------------------------------------------------------------------
nodes(_User, Arguments) ->
    case proplists:get_value(graph, Arguments) of
        undefined -> {error, {missing_argument, graph}};
        GraphRef  ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                Graph = graphbase_entity_obj:unref(Conn, GraphRef),
                Rules = proplists:get_value(rules, Arguments, []),
                {ok, [
                    graphbase_entity_obj:ref(Node) ||
                    Node <- graphbase_entity_graph:filter_nodes(Graph, Rules)
                ]}
            end)
    end.

%%--------------------------------------------------------------------
edges(_User, Arguments) ->
    case proplists:get_value(graph, Arguments) of
        undefined -> {error, {missing_argument, graph}};
        GraphRef  ->
            graphbase_backend_connection_pool:with(fun(Conn) ->
                Graph = graphbase_entity_obj:unref(Conn, GraphRef),
                Rules = proplists:get_value(rules, Arguments, []),
                {ok, [
                    graphbase_entity_obj:ref(Node) ||
                    Node <- graphbase_entity_graph:filter_edges(Graph, Rules)
                ]}
            end)
    end.
