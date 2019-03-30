-module(graphbase_entity_node).

%% API
-export([type/1, new/1, new/2]).

-include_lib("graphbase_entity/include/entity.hrl").

%%====================================================================
%% API functions
%%====================================================================

type(#entity{id = GraphId}) ->
    <<GraphId/binary, "_nodes">>.

%%--------------------------------------------------------------------
new(Graph) ->
    graphbase_entity_obj:new(type(Graph)).

%%--------------------------------------------------------------------
new(Id, Graph) ->
    graphbase_entity_obj:new(Id, type(Graph)).
