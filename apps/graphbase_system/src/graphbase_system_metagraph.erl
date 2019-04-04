-module(graphbase_system_metagraph).

%% API
-export([
    new/1,
    get_users/1,
    add_user/2,
    del_user/2,
    get_graphs/1,
    add_graph/2,
    del_graph/2,
    get_acls/1,
    add_acl/2,
    del_acl/2
]).

%%====================================================================
%% API
%%====================================================================

new(Conn) ->
    graphbase_entity_graph:new(Conn, <<"system">>).

%%--------------------------------------------------------------------
get_users(SystemGraph) ->
    graphbase_entity_graph:filter_nodes(SystemGraph, [
        {property, {<<"kind">>, register}, {eq, <<"user">>}}
    ]).

%%--------------------------------------------------------------------
add_user(SystemGraph, User) ->
    graphbase_entity_graph:add_nodes(SystemGraph, [User]).

%%--------------------------------------------------------------------
del_user(SystemGraph, User) ->
    graphbase_entity_graph:del_nodes(SystemGraph, [User]).

%%--------------------------------------------------------------------
get_graphs(SystemGraph) ->
    graphbase_entity_graph:filter_nodes(SystemGraph, [
        {property, {<<"kind">>, register}, {eq, <<"graph">>}}
    ]).

%%--------------------------------------------------------------------
add_graph(SystemGraph, Graph) ->
    graphbase_entity_graph:add_nodes(SystemGraph, [Graph]).

%%--------------------------------------------------------------------
del_graph(SystemGraph, Graph) ->
    graphbase_entity_graph:del_nodes(SystemGraph, [Graph]).

%%--------------------------------------------------------------------
get_acls(SystemGraph) ->
    graphbase_entity_graph:filter_edges(SystemGraph, [
        {property, {<<"relationship">>, register}, {eq, <<"acl">>}}
    ]).

%%--------------------------------------------------------------------
add_acl(SystemGraph, ACL) ->
    graphbase_entity_graph:add_edges(SystemGraph, [ACL]).

%%--------------------------------------------------------------------
del_acl(SystemGraph, ACL) ->
    graphbase_entity_graph:del_edges(SystemGraph, [ACL]).
