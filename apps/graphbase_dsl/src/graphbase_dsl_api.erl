%%%-------------------------------------------------------------------
%% @doc graphbase_dsl API module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_api).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    stop/2,
    interpret/2
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
    start_link([]).

%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
stop(API) ->
    stop(API, normal).

%%--------------------------------------------------------------------
stop(API, Reason) ->
    gen_server:cast(API, {stop, Reason}).

%%--------------------------------------------------------------------
interpret(API, Request) ->
    gen_server:call(API, {interpret, Request}).

%%====================================================================
%% Generic Server callbacks
%%====================================================================

init(Args) ->
    Timeout = proplists:get_value(timeout, Args, 5000),
    {ok, nostate, Timeout}.

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
    Reply = graphbase_core:with(
        fun() -> graphbase_dsl_ast:start_link() end,
        fun(Parser) -> graphbase_dsl_ast:stop(Parser) end,
        fun(Parser) ->
            graphbase_core:with(
                fun() -> graphbase_dsl_interpreter:start_link() end,
                fun(Interpreter) -> graphbase_dsl_interpreter:stop(Interpreter) end,
                fun(Interpreter) ->
                    case graphbase_dsl_ast:parse(Parser, Request) of
                        {ok, AST} ->
                            case graphbase_dsl_interpreter:run(Interpreter, AST) of
                                {ok, Response} -> {ok, Response};
                                Error          -> {error, {unable_to_interpret, Error}}
                            end;
                        
                        Error ->
                            {error, {unable_to_parse, Error}}
                    end
                end
            )
        end
    ),
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    {reply, {error, {invalid, Request}}, State}.

%%--------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State}.
