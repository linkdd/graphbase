-module(graphbase_system_graph).

%% API
-export([
    start_link/0,
    set_user/2,
    get_user/1,
    del_user/1,
    grant_read_access/2,
    grant_write_access/2,
    revoke_read_access/2,
    revoke_write_access/2,
    clear_read_access/1,
    clear_write_access/1,
    has_read_access/2,
    has_write_access/2
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

-include_lib("graphbase_entity/include/entity.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link(?SERVER, [], []).

%%--------------------------------------------------------------------
set_user(Name, Credentials) ->
    gen_server:call(?SERVER, {set_user, Name, Credentials}).

%%--------------------------------------------------------------------
get_user(Name) ->
    gen_server:call(?SERVER, {get_user, Name}).

%%--------------------------------------------------------------------
del_user(Name) ->
    gen_server:call(?SERVER, {del_user, Name}).

%%--------------------------------------------------------------------
grant_read_access(GraphRef, UserRef) ->
    gen_server:call(?SERVER, {grant_read_access, GraphRef, UserRef}).

%%--------------------------------------------------------------------
grant_write_access(GraphRef, UserRef) ->
    gen_server:call(?SERVER, {grant_write_access, GraphRef, UserRef}).

%%--------------------------------------------------------------------
revoke_read_access(GraphRef, UserRef) ->
    gen_server:call(?SERVER, {revoke_read_access, GraphRef, UserRef}).

%%--------------------------------------------------------------------
revoke_write_access(GraphRef, UserRef) ->
    gen_server:call(?SERVER, {revoke_write_access, GraphRef, UserRef}).

%%--------------------------------------------------------------------
clear_read_access(UserRef) ->
    gen_server:call(?SERVER, {clear_read_access, UserRef}).

%%--------------------------------------------------------------------
clear_write_access(UserRef) ->
    gen_server:call(?SERVER, {clear_write_access, UserRef}).

%%--------------------------------------------------------------------
has_read_access(GraphRef, UserRef) ->
    gen_server:call(?SERVER, {has_read_access, GraphRef, UserRef}).
    
%%--------------------------------------------------------------------
has_write_access(GraphRef, UserRef) ->
    gen_server:call(?SERVER, {has_write_access, GraphRef, UserRef}).

%%====================================================================
%% Generic Server callbacks
%%====================================================================

init(_Args) ->
    {ok, nostate}.

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

handle_call({set_user, Name, Credentials}, _From, State) ->
    {reply, graphbase_system_graph:set_user(Name, Credentials), State};

handle_call({get_user, Name}, _From, State) ->
    {reply, graphbase_system_graph:get_user(Name), State};

handle_call({del_user, Name}, _From, State) ->
    {reply, graphbase_system_graph:del_user(Name), State};

handle_call({grant_read_access, GraphRef, UserRef}, _From, State) ->
    {reply, graphbase_system_graph:grant_read_access(GraphRef, UserRef), State};

handle_call({grant_write_access, GraphRef, UserRef}, _From, State) ->
    {reply, graphbase_system_graph:grant_write_access(GraphRef, UserRef), State};

handle_call({revoke_read_access, GraphRef, UserRef}, _From, State) ->
    {reply, graphbase_system_graph:revoke_read_access(GraphRef, UserRef), State};

handle_call({revoke_write_access, GraphRef, UserRef}, _From, State) ->
    {reply, graphbase_system_graph:revoke_write_access(GraphRef, UserRef), State};

handle_call({clear_read_access, UserRef}, _From, State) ->
    {reply, graphbase_system_graph:clear_read_access(UserRef), State};

handle_call({clear_write_access, UserRef}, _From, State) ->
    {reply, graphbase_system_graph:clear_write_access(UserRef), State};

handle_call({has_read_access, GraphRef, UserRef}, _From, State) ->
    {reply, graphbase_system_graph:has_read_access(GraphRef, UserRef), State};

handle_call({has_write_access, GraphRef, UserRef}, _From, State) ->
    {reply, graphbase_system_graph:has_write_access(GraphRef, UserRef), State};

handle_call(Request, _From, State) ->
    {reply, {error, {invalid, Request}}, State}.

%%--------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State}.

%%====================================================================
%% Internal functions
%%====================================================================

