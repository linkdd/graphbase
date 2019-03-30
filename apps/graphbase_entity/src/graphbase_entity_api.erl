-module(graphbase_entity_api).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    stop/0,
    stop/1,
    ref/1,
    unref/1,
    fetch/1,
    save/1,
    delete/1
]).

%% Generic server callbacks
-export([
    init/1,
    code_change/3,
    terminate/2,
    handle_info/2,
    handle_call/3,
    handle_cast/2
]).

-define(SERVER, ?MODULE).

-include_lib("graphbase_entity/include/entity.hrl").
-record(state, {conn}).

%%====================================================================
%% API functions
%%====================================================================

start_link(Conn) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Conn, []).

%%--------------------------------------------------------------------
stop() ->
    stop(normal).

%%--------------------------------------------------------------------
stop(Reason) ->
    gen_server:cast(?SERVER, {stop, Reason}).

%%--------------------------------------------------------------------
ref(#entity{id = Id, type = Type}) ->
    term_to_binary({Id, Type}).

%%--------------------------------------------------------------------
unref(Ref) ->
    {Id, Type} = binary_to_term(Ref),
    fetch(#entity{id = Id, type = Type}).

%%--------------------------------------------------------------------
fetch(Entity) ->
    gen_server:call(?SERVER, {fetch, Entity}).

%%--------------------------------------------------------------------
save(Entity) ->
    gen_server:call(?SERVER, {save, Entity}).

%%--------------------------------------------------------------------
delete(Entity) ->
    gen_server:call(?SERVER, {delete, Entity}).

%%====================================================================
%% Generic Server callbacks
%%====================================================================

init(Conn) ->
    {ok, #state{conn = Conn}}.

%%--------------------------------------------------------------------
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {stop, timeout, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_call({stop, Reason}, _From, State) ->
    {stop, Reason, ok, State};

handle_call({fetch, Entity}, _From, State) ->
    {reply, {ok, handle_fetch(State#state.conn, Entity)}, State};

handle_call({save, Entity}, _From, State) ->
    {reply, {ok, handle_save(State#state.conn, Entity)}, State};
            
handle_call({delete, Entity}, _From, State) ->
    {reply, {ok, handle_delete(State#state.conn, Entity)}, State};

handle_call(Request, _From, State) ->
    {reply, {error, {invalid, Request}}, State}.

%%--------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_fetch(Conn, Entity) ->
    case graphbase_backend_connection:fetch_type(
        Conn,
        {<<"maps">>, Entity#entity.type},
        Entity#entity.id
    ) of
        {ok, Data}      -> {ok, Entity#entity{data = Data}};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
handle_save(Conn, #entity{id = Id, type = Type, data = Data}) ->
    {ok, NewData} = graphbase_backend_connection:update_type(
        Conn,
        {<<"maps">>, Type},
        Id,
        riakc_map:to_op(Data)
    ),
    {ok, #entity{id = Id, type = Type, data = NewData}}.

%%--------------------------------------------------------------------
handle_delete(Conn, #entity{id = Id, type = Type}) ->
    graphbase_backend_connection:delete(Conn, {<<"maps">>, Type}, Id).
