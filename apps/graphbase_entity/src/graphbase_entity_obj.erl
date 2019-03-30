%%%-------------------------------------------------------------------
%% @doc graphbase_entity Object module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_entity_obj).

%% API
-export([
    make_id/0,
    get_id/1,
    new/1,
    new/2,
    value/1,
    update/3
]).

-include_lib("graphbase_entity/include/entity.hrl").

%%====================================================================
%% API functions
%%====================================================================

make_id() ->
    Bin = term_to_binary({node(), eid:get_int()}),
    Hex = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]),
    list_to_binary(Hex).

%%--------------------------------------------------------------------
get_id(#entity{id = Id}) ->
    Id.

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
    Entity#entity{data = riakc_map:update(Key, Fun, Data)}.
