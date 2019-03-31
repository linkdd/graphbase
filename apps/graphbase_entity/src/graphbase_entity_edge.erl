-module(graphbase_entity_edge).

%% API
-export([
    type/1,
    new/2,
    new/3,
    add_neighbors/2,
    del_neighbors/2
]).

%%====================================================================
%% API functions
%%====================================================================

type(Graph) ->
    GraphId = graphbase_entity_obj:id(Graph),
    <<GraphId/binary, "_edges">>.

%%--------------------------------------------------------------------
new(Graph, NodeRef) ->
    Edge = graphbase_entity_obj:new(type(Graph)),
    graphbase_entity_obj:update(
        {<<"node">>, register},
        fun(R) -> riakc_register:set(NodeRef, R) end,
        Edge
    ).

%%--------------------------------------------------------------------
new(Id, Graph, NodeRef) ->
    Edge = graphbase_entity_obj:new(Id, type(Graph)),
    graphbase_entity_obj:update(
        {<<"node">>, register},
        fun(R) -> riakc_register:set(NodeRef, R) end,
        Edge
    ).

%%--------------------------------------------------------------------
add_neighbors(Edge, [NodeRef | NodeRefs]) ->
    add_neighbors(
        graphbase_entity_obj:update(
            {<<"neighbors">>, set},
            fun(S) -> riakc_set:add_element(NodeRef, S) end,
            Edge
        ),
        NodeRefs
    );

add_neighbors(Edge, []) ->
    Edge.

%%--------------------------------------------------------------------
del_neighbors(Edge, [NodeRef | NodeRefs]) ->
    del_neighbors(
        graphbase_entity_obj:update(
            {<<"neighbors">>, set},
            fun(S) -> riakc_set:del_element(NodeRef, S) end,
            Edge
        ),
        NodeRefs
    );

del_neighbors(Edge, []) ->
    Edge.
