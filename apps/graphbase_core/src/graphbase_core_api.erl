-module(graphbase_core_api).

%% API exports
-export([
    graph/2,
    nodes/2,
    edges/2
]).

%%====================================================================
%% API functions
%%====================================================================

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
