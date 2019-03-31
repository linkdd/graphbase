-module(graphbase_system_metagraph).

%% API
-export([
    get_or_create/1
]).

%%====================================================================
%% API
%%====================================================================

get_or_create(Conn) ->
    SystemGraphReq = graphbase_entity_graph:new(<<"system">>),
    {ok, SystemGraph} = graphbase_entity_api:save(Conn, SystemGraphReq),
    SystemGraph.
