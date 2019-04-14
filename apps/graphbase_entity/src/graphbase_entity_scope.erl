-module(graphbase_entity_scope).

%% API
-export([
    graph/1,
    set_graph/2
]).

%%====================================================================
%% API functions
%%====================================================================

graph(Entity) ->
    proplists:get_value(
        {<<"graph">>, register},
        graphbase_entity_obj:value(Entity)
    ).

%%--------------------------------------------------------------------
set_graph(Entity, Graph) ->
    graphbase_entity_obj:update(
        Entity,
        {<<"graph">>, register},
        fun(R) -> riakc_register:set(graphbase_entity_obj:ref(Graph), R) end
    ).
