-module(graphbase_entity_graph).

-export([
    type/0,
    new/0,
    new/1,
    add_nodes/2,
    del_nodes/2,
    add_edges/2,
    del_edges/2
]).

%%====================================================================
%% API functions
%%====================================================================

type() ->
    <<"graphs">>.

%%--------------------------------------------------------------------
new() ->
    graphbase_entity_obj:new(type()).

%%--------------------------------------------------------------------
new(Id) ->
    graphbase_entity_obj:new(Id, type()).

%%--------------------------------------------------------------------
add_nodes(Graph, [Node | Nodes]) ->
    add_nodes(
        graphbase_entity_obj:update(
            {<<"nodes">>, set},
            fun(S) -> riakc_set:add_element(graphbase_entity_obj:ref(Node), S) end,
            Graph
        ),
        Nodes
    );

add_nodes(Graph, []) ->
    Graph.

%%--------------------------------------------------------------------
del_nodes(Graph, [Node | Nodes]) ->
    del_nodes(
        graphbase_entity_obj:update(
            {<<"nodes">>, set},
            fun(S) -> riakc_set:del_element(graphbase_entity_obj:ref(Node), S) end,
            Graph
        ),
        Nodes
    );

del_nodes(Graph, []) ->
    Graph.

%%--------------------------------------------------------------------
add_edges(Graph, [Edge | Edges]) ->
    add_edges(
        graphbase_entity_obj:update(
            {<<"edges">>, set},
            fun(S) -> riakc_set:add_element(graphbase_entity_obj:ref(Edge), S) end,
            Graph
        ),
        Edges
    );

add_edges(Graph, []) ->
    Graph.

%%--------------------------------------------------------------------
del_edges(Graph, [Edge | Edges]) ->
    del_edges(
        graphbase_entity_obj:update(
            {<<"edges">>, set},
            fun(S) -> riakc_set:del_element(graphbase_entity_obj:ref(Edge), S) end,
            Graph
        ),
        Edges
    );

del_edges(Graph, []) ->
    Graph.
