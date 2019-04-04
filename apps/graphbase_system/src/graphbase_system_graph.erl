-module(graphbase_system_graph).

%% API
-export([
    new/2,
    entity/1
]).

%%====================================================================
%% API
%%====================================================================

new(Conn, SystemGraph) ->
    GraphId = graphbase_entity_obj:id(graphbase_entity_graph:new(Conn)),
    Graph0 = graphbase_entity_node:new(Conn, <<"graph_", GraphId/binary>>, SystemGraph),
    Graph1 = graphbase_entity_node:set_kind(Graph0, <<"graph">>),
    graphbase_entity_obj:update(
        Graph1,
        {<<"graph_id">>, register},
        fun(R) -> riakc_register:set(GraphId, R) end
    ).

%%--------------------------------------------------------------------
entity(Graph) ->
    case proplists:get_value({<<"graph_id">>, register}, graphbase_entity_obj:value(Graph)) of
        undefined -> {error, {undefined, graph_id}};
        GraphId   -> {ok, graphbase_entity_graph:new(graphbase_entity_obj:conn(Graph), GraphId)}
    end.
