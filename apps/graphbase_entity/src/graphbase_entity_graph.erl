-module(graphbase_entity_graph).

-export([
    new/1,
    new/2,
    get_nodes/1,
    filter_nodes/2,
    add_nodes/2,
    del_nodes/2,
    get_edges/1,
    filter_edges/2,
    add_edges/2,
    del_edges/2
]).

%%====================================================================
%% API functions
%%====================================================================

new(Conn) ->
    graphbase_entity_obj:new(Conn, type()).

%%--------------------------------------------------------------------
new(Conn, Id) ->
    graphbase_entity_obj:new(Conn, Id, type()).

%%--------------------------------------------------------------------
get_nodes(Graph) ->
    [graphbase_entity_obj:unref(graphbase_entity_obj:conn(Graph), Ref) || Ref <- proplists:get_value(
        {<<"nodes">>, set},
        graphbase_entity_obj:value(Graph),
        []
    )].

%%--------------------------------------------------------------------
filter_nodes(Graph, Rules) ->
    {ok, P} = emapred_pipeline:new(
        fun(Node) ->
            case graphbase_core_filter:match(Rules, graphbase_entity_obj:value(Node)) of
                true  -> {emit, {node, Node}};
                false -> ok
            end
        end,
        fun(node, Node, Result) ->
            {ok, [Node | Result]}
        end,
        []
    ),
    lists:foreach(
        fun(Node) ->
            ok = emapred_pipeline:send(P, Node),
            ok
        end,
        get_nodes(Graph)
    ),
    emapred_pipeline:stop(P).

%%--------------------------------------------------------------------
add_nodes(Graph, [Node | Nodes]) ->
    add_nodes(
        graphbase_entity_obj:update(
            Graph,
            {<<"nodes">>, set},
            fun(S) -> riakc_set:add_element(graphbase_entity_obj:ref(Node), S) end
        ),
        Nodes
    );

add_nodes(Graph, []) ->
    Graph.

%%--------------------------------------------------------------------
del_nodes(Graph, [Node | Nodes]) ->
    del_nodes(
        graphbase_entity_obj:update(
            Graph,
            {<<"nodes">>, set},
            fun(S) -> riakc_set:del_element(graphbase_entity_obj:ref(Node), S) end
        ),
        Nodes
    );

del_nodes(Graph, []) ->
    Graph.

%%--------------------------------------------------------------------
get_edges(Graph) ->
    [graphbase_entity_obj:unref(graphbase_entity_obj:conn(Graph), Ref) || Ref <- proplists:get_value(
        {<<"edges">>, set},
        graphbase_entity_obj:value(Graph),
        []
    )].

%%--------------------------------------------------------------------
filter_edges(Graph, Rules) ->
    {ok, P} = emapred_pipeline:new(
        fun(Edge) ->
            case graphbase_core_filter:match(Rules, graphbase_entity_obj:value(Edge)) of
                true  -> {emit, {edge, Edge}};
                false -> ok
            end
        end,
        fun(edge, Edge, Result) ->
            {ok, [Edge | Result]}
        end,
        []
    ),
    lists:foreach(
        fun(Edge) ->
            ok = emapred_pipeline:send(P, Edge),
            ok
        end,
        get_edges(Graph)
    ),
    emapred_pipeline:stop(P).

%%--------------------------------------------------------------------
add_edges(Graph, [Edge | Edges]) ->
    add_edges(
        graphbase_entity_obj:update(
            Graph,
            {<<"edges">>, set},
            fun(S) -> riakc_set:add_element(graphbase_entity_obj:ref(Edge), S) end
        ),
        Edges
    );

add_edges(Graph, []) ->
    Graph.

%%--------------------------------------------------------------------
del_edges(Graph, [Edge | Edges]) ->
    del_edges(
        graphbase_entity_obj:update(
            Graph,
            {<<"edges">>, set},
            fun(S) -> riakc_set:del_element(graphbase_entity_obj:ref(Edge), S) end
        ),
        Edges
    );

del_edges(Graph, []) ->
    Graph.

%%====================================================================
%% Internal functions
%%====================================================================

type() ->
    <<"graphs">>.
