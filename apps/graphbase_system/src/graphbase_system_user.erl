-module(graphbase_system_user).

%% API
-export([
    new/3,
    name/1,
    credentials/1,
    set_credentials/2
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

%%--------------------------------------------------------------------
credentials(User) ->
    case proplists:get_value({<<"credentials">>, register}, graphbase_entity_obj:value(User)) of
        undefined -> undefined;
        Value     -> binary_to_term(Value)
    end.

%%--------------------------------------------------------------------
set_credentials(User, Credentials) ->
    graphbase_entity_obj:update(
        User,
        {<<"credentials">>, register},
        fun(R) -> riakc_register:set(term_to_binary(Credentials), R) end
    ).
