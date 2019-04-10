-module(graphbase_entity_obj).

-export([
    new/2,
    new/3,
    conn/1,
    id/1,
    type/1,
    data/1,
    ref/1,
    unref/2,
    fetch/1,
    save/1,
    delete/1,
    value/1,
    update/3
]).

-record(entity, {conn, id, type, data}).

%%====================================================================
%% API functions
%%====================================================================

new(Conn, Type) ->
    new(Conn, make_id(), Type).

%%--------------------------------------------------------------------
new(Conn, Id, Type) ->
    #entity{
        conn = Conn,
        id = Id,
        type = Type,
        data = riakc_map:new()
    }.

%%--------------------------------------------------------------------
conn(#entity{conn = Conn}) ->
    Conn.

%%--------------------------------------------------------------------
id(#entity{id = Id}) ->
    Id.

%%--------------------------------------------------------------------
type(#entity{type = Type}) ->
    Type.

%%--------------------------------------------------------------------
data(#entity{data = Data}) ->
    Data.

%%--------------------------------------------------------------------
ref(#entity{id = Id, type = Type}) ->
    term_to_binary({Id, Type}).

%%--------------------------------------------------------------------
unref(Conn, Ref) ->
    {Id, Type} = binary_to_term(Ref),
    new(Conn, Id, Type).

%%--------------------------------------------------------------------
fetch(Entity = #entity{conn = Conn, id = Id, type = Type}) ->
    case graphbase_backend_connection:fetch_type(Conn, bucket_for_type(Type), Id) of
        {ok, Data}      -> {ok, with_data(Entity, Data)};
        {error, Reason} -> {error, {unable_to_fetch, Reason}}
    end.

%%--------------------------------------------------------------------
save(Entity = #entity{conn = Conn, id = Id, type = Type, data = Data}) ->
    case riakc_map:to_op(Data) of
        undefined ->
            {ok, Entity};
        Operation ->
            case graphbase_backend_connection:update_type(Conn, bucket_for_type(Type), Id, Operation) of
                {ok, NewData} ->
                    {ok, with_data(Entity, NewData)};
                {error, unmodified} ->
                    {ok, Entity};
                {error, Reason} ->
                    {error, {unable_to_save, Reason}}
            end
    end.

%%--------------------------------------------------------------------
delete(#entity{conn = Conn, id = Id, type = Type}) ->
    case graphbase_backend_connection:delete(Conn, bucket_for_type(Type), Id) of
        ok              -> ok;
        {error, Reason} -> {error, {unable_to_delete, Reason}}
    end.

%%--------------------------------------------------------------------
value(Entity) ->
    case fetch(Entity) of
        {ok, #entity{data = Data}} -> riakc_map:value(Data);
        Error                      -> Error
    end.

%%--------------------------------------------------------------------
update(Entity = #entity{data = Data}, Key, Fun) ->
    with_data(Entity, riakc_map:update(Key, Fun, Data)).

%%====================================================================
%% API functions
%%====================================================================

make_id() ->
    Bin = term_to_binary({node(), eid:get_int()}),
    Hex = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]),
    list_to_binary(Hex).

%%--------------------------------------------------------------------
bucket_for_type(Type) ->
    {<<"maps">>, Type}.

%%--------------------------------------------------------------------
with_data(Entity, Data) ->
    Entity#entity{data = Data}.
