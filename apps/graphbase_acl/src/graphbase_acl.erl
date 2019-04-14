-module(graphbase_acl).

%% API
-export([
    new/4,
    user/1,
    access/1,
    has/2,
    grant/2,
    revoke/2
]).

%%====================================================================
%% API
%%====================================================================

new(Conn, MetaGraph, User, Access) ->
    UserName = graphbase_acl_user:name(User),
    BAccess = list_to_binary(atom_to_list(Access)),
    Id = <<"acl_", BAccess/binary, "_", UserName/binary>>,
    ACL0 = graphbase_entity_edge:new(Conn, Id, MetaGraph, User),
    ACL1 = graphbase_entity_edge:set_relationship(ACL0, <<"acl">>),
    graphbase_entity_obj:update(
        ACL1,
        {<<"access">>, register},
        fun(R) -> riakc_register:set(BAccess, R) end
    ).

%%--------------------------------------------------------------------
user(ACL) ->
    graphbase_entity_edge:node(ACL).

%%--------------------------------------------------------------------
access(ACL) ->
    case proplists:get_value({<<"access">>, register}, graphbase_entity_obj:value(ACL)) of
        undefined   -> {error, {undefined, access}};
        <<"read">>  -> read;
        <<"write">> -> write
    end.

%%--------------------------------------------------------------------
has(ACL, Entity) ->
    {ok, P} = emapred_pipeline:new(
        fun(N) ->
            case graphbase_entity_obj:ref(Entity) == graphbase_entity_obj:ref(N) of
                true  -> {emit, {match, N}};
                false -> {emit, {nomatch, N}}
            end
        end,
        fun(Key, _N, Acc) ->
            case Key of
                match   -> {ok, true};
                nomatch -> {ok, Acc}
            end
        end,
        false
    ),
    lists:foreach(
        fun(N) ->
            ok = emapred_pipeline:send(P, N),
            ok
        end,
        graphbase_entity_edge:get_neighbors(ACL)
    ),
    emapred_pipeline:stop(P).

%%--------------------------------------------------------------------
grant(ACL, Entity) ->
    graphbase_entity_edge:add_neighbors(ACL, [Entity]).

%%--------------------------------------------------------------------
revoke(ACL, Entity) ->
    graphbase_entity_edge:del_neighbors(ACL, [Entity]).
