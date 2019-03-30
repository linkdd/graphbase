-module(graphbase_entity_graph).

-export([
    type/0,
    new/0,
    new/1,
    add_nodes/2,
    del_nodes/2
]).

-include_lib("graphbase_entity/include/entity.hrl").

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
        Graph#entity{
            data = riakc_map:update(
                {<<"nodes">>, set},
                fun(S) -> riakc_set:add_element(graphbase_entity_obj:ref(Node), S) end,
                Graph#entity.data
            )
        },
        Nodes
    );

add_nodes(Graph, []) ->
    Graph.

%%--------------------------------------------------------------------
del_nodes(Graph, [Node | Nodes]) ->
    del_nodes(
        Graph#entity{
            data = riakc_map:update(
                {<<"nodes">>, set},
                fun(S) -> riakc_set:del_element(graphbase_entity_obj:ref(Node), S) end,
                Graph#entity.data
            )
        },
        Nodes
    );

del_nodes(Graph, []) ->
    Graph.

