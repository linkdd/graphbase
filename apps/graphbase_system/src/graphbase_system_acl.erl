-module(graphbase_system_acl).

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

new(Conn, SystemGraph, User, Access) ->
    UserName = graphbase_system_user:name(User),
    Id = <<"acl_", Access/binary, "_", UserName/binary>>,
    ACL0 = graphbase_entity_edge:new(Conn, Id, SystemGraph, User),
    ACL1 = graphbase_entity_edge:set_relationship(ACL0, <<"acl">>),
    graphbase_entity_obj:update(
        ACL1,
        {<<"access">>, register},
        fun(R) -> riakc_register:set(atom_to_list(list_to_binary(Access)), R) end
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
has(ACL, Graph) ->
    {ok, P} = emapred_pipeline:new(
        fun(N) ->
            case graphbase_entity_obj:ref(Graph) == graphbase_entity_obj:ref(N) of
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
grant(ACL, Graph) ->
    graphbase_entity_edge:add_neighbors(ACL, [Graph]).

%%--------------------------------------------------------------------
revoke(ACL, Graph) ->
    graphbase_entity_edge:del_neighbors(ACL, [Graph]).
