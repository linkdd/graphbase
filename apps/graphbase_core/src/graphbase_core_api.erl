-module(graphbase_core_api).

%% API exports
-export([
    debug/1,
    list_users/1,
    list_acls/1,
    list_graphs/1
]).

%%====================================================================
%% API functions
%%====================================================================

debug(Arguments) ->
    io:format("~p~n", [Arguments]),
    {ok, debug}.

%%--------------------------------------------------------------------
list_users(_Arguments) ->
    graphbase_core:with(
        fun() -> graphbase_backend_connection_pool:acquire() end,
        fun(Conn) -> graphbase_backend_connection_pool:release(Conn) end,
        fun(Conn) ->
            MetaGraph = graphbase_system_metagraph:new(Conn),
            Users = graphbase_system_metagraph:get_users(MetaGraph),
            {ok, {nodes, [
                #{
                    id => graphbase_entity_obj:id(U),
                    properties => graphbase_entity_obj:value(U)
                } || U <- Users
            ]}}
        end
    ).

%%--------------------------------------------------------------------
list_acls(_Arguments) ->
    graphbase_core:with(
        fun() -> graphbase_backend_connection_pool:acquire() end,
        fun(Conn) -> graphbase_backend_connection_pool:release(Conn) end,
        fun(Conn) ->
            MetaGraph = graphbase_system_metagraph:new(Conn),
            ACLs = graphbase_system_metagraph:get_acls(MetaGraph),
            {ok, {edges, [
                #{
                    id => graphbase_entity_obj:id(U),
                    properties => graphbase_entity_obj:value(U)
                } || U <- ACLs
            ]}}
        end
    ).

%%--------------------------------------------------------------------
list_graphs(_Arguments) ->
    graphbase_core:with(
        fun() -> graphbase_backend_connection_pool:acquire() end,
        fun(Conn) -> graphbase_backend_connection_pool:release(Conn) end,
        fun(Conn) ->
            MetaGraph = graphbase_system_metagraph:new(Conn),
            Graphs = graphbase_system_metagraph:get_graphs(MetaGraph),
            {ok, {nodes, [
                #{
                    id => graphbase_entity_obj:id(U),
                    properties => graphbase_entity_obj:value(U)
                } || U <- Graphs
            ]}}
        end
    ).
