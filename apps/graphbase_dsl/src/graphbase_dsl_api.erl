%%%-------------------------------------------------------------------
%% @doc graphbase_dsl API module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_api).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    stop/0,
    stop/1,
    interpret/1
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

%%--------------------------------------------------------------------
stop() ->
    stop(normal).

%%--------------------------------------------------------------------
stop(Reason) ->
    gen_server:cast(?SERVER, {stop, Reason}).

%%--------------------------------------------------------------------
interpret(Request) ->
    gen_server:call(?SERVER, {interpret, Request}).

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

handle_call({interpret, Request}, _From, State) ->
    {reply, handle_interpret(Request), State};

handle_call(Request, _From, State) ->
    {reply, {error, {invalid, Request}}, State}.

%%--------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_interpret(Request) ->
    {ok, AST} = graphbase_dsl_ast:parse(Request),
    graphbase_dsl_interpreter:run(AST).
