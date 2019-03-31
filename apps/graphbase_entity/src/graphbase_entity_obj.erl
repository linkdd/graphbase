%%%-------------------------------------------------------------------
%% @doc graphbase_entity Object module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_entity_obj).

%% API
-export([
    type/1,
    id/1,
    ref/1,
    unref/1,
    new/1,
    new/2,
    value/1,
    update/3,
    with_data/2,
    op/1
]).

-record(entity, {id, type, data}).

%%====================================================================
%% API functions
%%====================================================================

type(#entity{type = Type}) ->
    Type.

%%--------------------------------------------------------------------
id(#entity{id = Id}) ->
    Id.

%%--------------------------------------------------------------------
ref(#entity{id = Id, type = Type}) ->
    term_to_binary({Id, Type}).

%%--------------------------------------------------------------------
unref(Ref) ->
    {Id, Type} = binary_to_term(Ref),
    #entity{id = Id, type = Type}.

%%--------------------------------------------------------------------
new(Type) ->
    new(make_id(), Type).

%%--------------------------------------------------------------------
new(Id, Type) ->
    #entity{
        id = Id,
        type = <<"graphbase_", Type/binary>>,
        data = riakc_map:new()
    }.

%%--------------------------------------------------------------------
value(#entity{data = Data}) ->
    riakc_map:value(Data).

%%--------------------------------------------------------------------
update(Key, Fun, Entity = #entity{data = Data}) ->
    with_data(riakc_map:update(Key, Fun, Data), Entity).

%%--------------------------------------------------------------------
with_data(Data, Entity) ->
    Entity#entity{data = Data}.

%%--------------------------------------------------------------------
op(#entity{data = Data}) ->
    riakc_map:to_op(Data).

%%====================================================================
%% Internal functions
%%====================================================================

make_id() ->
    Bin = term_to_binary({node(), eid:get_int()}),
    Hex = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]),
    list_to_binary(Hex).
