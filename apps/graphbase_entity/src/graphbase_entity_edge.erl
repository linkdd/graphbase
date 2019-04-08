-module(graphbase_entity_edge).

%% API
-export([
    new/3,
    new/4,
    node/1,
    relationship/1,
    set_relationship/2,
    get_neighbors/1,
    add_neighbors/2,
    del_neighbors/2
]).

%%====================================================================
%% API functions
%%====================================================================

new(Conn, Graph, Node) ->
    Edge = graphbase_entity_obj:new(Conn, type(Graph)),
    graphbase_entity_obj:update(
        Edge,
        {<<"node">>, register},
        fun(R) -> riakc_register:set(graphbase_entity_obj:ref(Node), R) end
    ).

%%--------------------------------------------------------------------
new(Conn, Id, Graph, Node) ->
    Edge = graphbase_entity_obj:new(Conn, Id, type(Graph)),
    graphbase_entity_obj:update(
        Edge,
        {<<"node">>, register},
        fun(R) -> riakc_register:set(graphbase_entity_obj:ref(Node), R) end
    ).

%%--------------------------------------------------------------------
node(Edge) ->
    case proplists:get_value({<<"node">>, register}, graphbase_entity_obj:value(Edge)) of
        undefined -> {error, {undefined, node}};
        NodeRef   -> graphbase_entity_obj:unref(graphbase_entity_obj:conn(Edge), NodeRef)
    end.

%%--------------------------------------------------------------------
relationship(Edge) ->
    proplists:get_value(
        {<<"relationship">>, register},
        graphbase_entity_obj:value(Edge)
    ).

%%--------------------------------------------------------------------
set_relationship(Edge, Relationship) ->
    graphbase_entity_obj:update(
        Edge,
        {<<"relationship">>, register},
        fun(R) -> riakc_register:set(Relationship, R) end
    ).

%%--------------------------------------------------------------------
get_neighbors(Edge) ->
    [graphbase_entity_obj:unref(graphbase_entity_obj:conn(Edge), Ref) || Ref <- proplists:get_value(
        {<<"neighbors">>, set},
        graphbase_entity_obj:value(Edge),
        []
    )].

%%--------------------------------------------------------------------
add_neighbors(Edge, [Node | Nodes]) ->
    add_neighbors(
        graphbase_entity_obj:update(
            Edge,
            {<<"neighbors">>, set},
            fun(S) -> riakc_set:add_element(graphbase_entity_obj:ref(Node), S) end
        ),
        Nodes
    );

add_neighbors(Edge, []) ->
    Edge.

%%--------------------------------------------------------------------
del_neighbors(Edge, [Node | Nodes]) ->
    del_neighbors(
        graphbase_entity_obj:update(
            Edge,
            {<<"neighbors">>, set},
            fun(S) -> riakc_set:del_element(graphbase_entity_obj:ref(Node), S) end
        ),
        Nodes
    );

del_neighbors(Edge, []) ->
    Edge.

%%====================================================================
%% Internal functions
%%====================================================================

type(Graph) ->
    GraphId = graphbase_entity_obj:id(Graph),
    <<GraphId/binary, "_edges">>.
