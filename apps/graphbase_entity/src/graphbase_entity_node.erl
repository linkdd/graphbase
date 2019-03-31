-module(graphbase_entity_node).

%% API
-export([type/1, new/1, new/2]).

%%====================================================================
%% API functions
%%====================================================================

type(Graph) ->
    GraphId = graphbase_entity_obj:id(Graph),
    <<GraphId/binary, "_nodes">>.

%%--------------------------------------------------------------------
new(Graph) ->
    graphbase_entity_obj:new(type(Graph)).

%%--------------------------------------------------------------------
new(Id, Graph) ->
    graphbase_entity_obj:new(Id, type(Graph)).
