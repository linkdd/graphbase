-module(graphbase_acl_user).

%% API
-export([
    new/3,
    name/1
]).

%%====================================================================
%% API
%%====================================================================

new(Conn, MetaGraph, Name) ->
    User0 = graphbase_entity_node:new(Conn, <<"user_", Name/binary>>, MetaGraph),
    User1 = graphbase_entity_node:set_kind(User0, <<"user">>),
    graphbase_entity_obj:update(
        User1,
        {<<"name">>, register},
        fun(R) -> riakc_register:set(Name, R) end
    ).

%%--------------------------------------------------------------------
name(User) ->
    case proplists:get_value({<<"name">>, register}, graphbase_entity_obj:value(User)) of
        undefined -> undefined;
        Value     -> Value
    end.
