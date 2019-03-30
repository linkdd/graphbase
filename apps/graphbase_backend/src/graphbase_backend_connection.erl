%%%-------------------------------------------------------------------
%% @doc graphbase_backend Connection module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_backend_connection).

-behaviour(gen_server).

%% API
-export([
    start_link/3,
    stop/1,
    stop/2,
    create_search_schema/3,
    get_search_schema/2,
    fetch_type/3,
    update_type/4,
    delete/3
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

-record(state, {conn}).

%%====================================================================
%% API functions
%%====================================================================

start_link(Host, Port, Options) ->
    gen_server:start_link(?MODULE, {Host, Port, Options}, []).

%%--------------------------------------------------------------------
stop(Conn) ->
    stop(normal, Conn).

%%--------------------------------------------------------------------
stop(Reason, Conn) ->
    gen_server:cast(Conn, {stop, Reason}).

%%--------------------------------------------------------------------
create_search_schema(Conn, SchemaName, SchemaData) ->
    gen_server:call(Conn, {create_search_schema, SchemaName, SchemaData}).

%%--------------------------------------------------------------------
get_search_schema(Conn, SchemaName) ->
    gen_server:call(Conn, {get_search_schema, SchemaName}).

%%--------------------------------------------------------------------
fetch_type(Conn, Bucket, Key) ->
    gen_server:call(Conn, {fetch_type, Bucket, Key}).

%%--------------------------------------------------------------------
update_type(Conn, Bucket, Key, Operation) ->
    gen_server:call(Conn, {update_type, Bucket, Key, Operation}).

%%--------------------------------------------------------------------
delete(Conn, Bucket, Key) ->
    gen_server:call(Conn, {delete, Bucket, Key}).

%%====================================================================
%% Generic Server callbacks
%%====================================================================

init({Host, Port, Options0}) ->
    Options = case proplists:get_value(auto_reconnect, Options0) of
        undefined -> [{auto_reconnect, true} | Options0];
        _         -> Options0
    end,
    {ok, Conn} = riakc_pb_socket:start_link(Host, Port, Options),
    {ok, #state{conn = Conn}}.

%%--------------------------------------------------------------------
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
terminate(_Reason, #state{conn = Conn}) ->
    riakc_pb_socket:stop(Conn).

%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {stop, timeout, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_call({stop, Reason}, _From, State) ->
    {stop, Reason, ok, State};

handle_call({create_search_schema, SchemaName, SchemaData}, _From, State) ->
    {reply, riakc_pb_socket:create_search_schema(State#state.conn, SchemaName, SchemaData), State};

handle_call({get_search_schema, SchemaName}, _From, State) ->
    {reply, riakc_pb_socket:get_search_schema(State#state.conn, SchemaName), State};
                    
handle_call({fetch_type, Bucket, Key}, _From, State) ->
    {reply, riakc_pb_socket:fetch_type(State#state.conn, Bucket, Key), State};
        
handle_call({update_type, Bucket, Key, Operation}, _From, State) ->
    {reply, riakc_pb_socket:update_type(State#state.conn, Bucket, Key, Operation, [{return_body, true}]), State};

handle_call({delete, Bucket, Key}, _From, State) ->
    {reply, riakc_pb_socket:delete(State#state.conn, Bucket, Key), State};

handle_call(Request, _From, State) ->
    {reply, {error, {invalid, Request}}, State}.

%%--------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State}.
