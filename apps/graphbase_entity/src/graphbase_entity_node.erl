-module(graphbase_entity_node).

%% API
-export([
    new/2,
    new/3,
    kind/1,
    set_kind/2
]).

%%====================================================================
%% API functions
%%====================================================================

new(Conn, Graph) ->
    graphbase_entity_scope:set_graph(graphbase_entity_obj:new(Conn, type(Graph)), Graph).

%%--------------------------------------------------------------------
new(Conn, Id, Graph) ->
    graphbase_entity_scope:set_graph(graphbase_entity_obj:new(Conn, Id, type(Graph)), Graph).

%%--------------------------------------------------------------------
kind(Node) ->
    proplists:get_value(
        {<<"kind">>, register},
        graphbase_entity_obj:value(Node)
    ).

%%--------------------------------------------------------------------
set_kind(Node, Kind) ->
    graphbase_entity_obj:update(
        Node,
        {<<"kind">>, register},
        fun(R) -> riakc_register:set(Kind, R) end
    ).

%%====================================================================
%% Internal functions
%%====================================================================

type(Graph) ->
    GraphId = graphbase_entity_obj:id(Graph),
    <<GraphId/binary, "_nodes">>.