-module(graphbase_metagraph_graph).

%% API
-export([
    new/2,
    new/3,
    entity/1
]).

%%====================================================================
%% API
%%====================================================================

new(Conn, MetaGraph) ->
    new(Conn, graphbase_entity_obj:id(graphbase_entity_graph:new(Conn)), MetaGraph).

%%--------------------------------------------------------------------
new(Conn, GraphId, MetaGraph) ->
    Graph0 = graphbase_entity_node:new(Conn, <<"graph_", GraphId/binary>>, MetaGraph),
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
