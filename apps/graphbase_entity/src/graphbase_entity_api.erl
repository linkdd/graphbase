-module(graphbase_entity_api).

%% API
-export([
    fetch_ref/2,
    fetch/2,
    save/2,
    delete/2
]).

%%====================================================================
%% API functions
%%====================================================================

fetch_ref(Conn, Ref) ->
    fetch(Conn, graphbase_entity_obj:unref(Ref)).

%%--------------------------------------------------------------------
fetch(Conn, Entity) ->
    case graphbase_backend_connection:fetch_type(
        Conn,
        {<<"maps">>, graphbase_entity_obj:type(Entity)},
        graphbase_entity_obj:id(Entity)
    ) of
        {ok, Data}      -> {ok, graphbase_entity_obj:with_data(Data, Entity)};
        {error, Reason} -> {error, Reason}
    end.
    
%%--------------------------------------------------------------------
save(Conn, Entity) ->
    {ok, NewData} = graphbase_backend_connection:update_type(
        Conn,
        {<<"maps">>, graphbase_entity_obj:type(Entity)},
        graphbase_entity_obj:id(Entity),
        graphbase_entity_obj:op(Entity)
    ),
    {ok, graphbase_entity_obj:with_data(NewData, Entity)}.
    
%%--------------------------------------------------------------------
delete(Conn, Entity) ->
    graphbase_backend_connection:delete(
        Conn,
        {<<"maps">>, graphbase_entity_obj:type(Entity)},
        graphbase_entity_obj:id(Entity)
    ).
