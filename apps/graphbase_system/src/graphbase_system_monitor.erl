-module(graphbase_system_monitor).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    stats/0
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

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stats() ->
    gen_server:call(?SERVER, stats).

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
handle_call(stats, _From, State) ->
    Reply = [
        {graphbase_backend_connection_total, graphbase_backend_connection_pool:count_children(), []}
    ],
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    {reply, {error, {invalid, Request}}, State}.

%%--------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State}.
