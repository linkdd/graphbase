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
get_users(MetaGraph) ->
    graphbase_entity_graph:filter_nodes(MetaGraph, [
        {property, {<<"kind">>, register}, {eq, <<"user">>}}
    ]).

%%--------------------------------------------------------------------
add_user(MetaGraph, User) ->
    graphbase_entity_graph:add_nodes(MetaGraph, [User]).

%%--------------------------------------------------------------------
del_user(MetaGraph, User) ->
    graphbase_entity_graph:del_nodes(MetaGraph, [User]).

%%--------------------------------------------------------------------
get_graphs(MetaGraph) ->
    graphbase_entity_graph:filter_nodes(MetaGraph, [
        {property, {<<"kind">>, register}, {eq, <<"graph">>}}
    ]).

%%--------------------------------------------------------------------
add_graph(MetaGraph, Graph) ->
    graphbase_entity_graph:add_nodes(MetaGraph, [Graph]).

%%--------------------------------------------------------------------
del_graph(MetaGraph, Graph) ->
    graphbase_entity_graph:del_nodes(MetaGraph, [Graph]).

%%--------------------------------------------------------------------
get_acls(MetaGraph) ->
    graphbase_entity_graph:filter_edges(MetaGraph, [
        {property, {<<"relationship">>, register}, {eq, <<"acl">>}}
    ]).

%%--------------------------------------------------------------------
add_acl(MetaGraph, ACL) ->
    graphbase_entity_graph:add_edges(MetaGraph, [ACL]).

%%--------------------------------------------------------------------
del_acl(MetaGraph, ACL) ->
    graphbase_entity_graph:del_edges(MetaGraph, [ACL]).
