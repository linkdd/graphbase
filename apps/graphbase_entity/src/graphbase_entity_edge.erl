-module(graphbase_entity_edge).

%% API
-export([
    type/1,
    new/2,
    new/3,
    add_neighbors/2,
    del_neighbors/2
]).

-include_lib("graphbase_entity/include/entity.hrl").

%%====================================================================
%% API functions
%%====================================================================

type(#entity{id = GraphId}) ->
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
        Edge#entity{
            data = riakc_map:update(
                {<<"neighbors">>, set},
                fun(S) -> riakc_set:add_element(NodeRef, S) end,
                Edge#entity.data
            )
        },
        NodeRefs
    );

add_neighbors(Edge, []) ->
    Edge.

%%--------------------------------------------------------------------
del_neighbors(Edge, [NodeRef | NodeRefs]) ->
    del_neighbors(
        Edge#entity{
            data = riakc_map:update(
                {<<"neighbors">>, set},
                fun(S) -> riakc_set:del_element(NodeRef, S) end,
                Edge#entity.data
            )
        },
        NodeRefs
    );

del_neighbors(Edge, []) ->
    Edge.
