%%%-------------------------------------------------------------------
%% @doc graphbase_dsl Interpreter module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_interpreter).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    stop/2,
    run/2
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

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
stop(Interpreter) ->
    stop(Interpreter, normal).

%%--------------------------------------------------------------------
stop(Interpreter, Reason) ->
    gen_server:cast(Interpreter, {stop, Reason}).

%%--------------------------------------------------------------------
run(Interpreter, AST) ->
    gen_server:call(Interpreter, {run, AST}).

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

handle_call({run, AST}, _From, State) ->
    {reply, execute(AST), State};

handle_call(Request, _From, State) ->
    {reply, {error, {invalid, Request}}, State}.

%%--------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State}.

%%====================================================================
%% Internal functions
%%====================================================================

execute(AST) ->
    execute(AST, dict:new(), dict:new()).

%%--------------------------------------------------------------------
execute([Statement | Statements], Scope, Response) ->
    case execute_statement(Statement, Scope, Response) of
        {ok, NewScope, NewResponse} ->
            execute(Statements, NewScope, NewResponse);
        Error ->
            {error, {statement_error, Error}}
    end;

execute([], _Scope, Response) ->
    {ok, dict:to_list(Response)}.

%%--------------------------------------------------------------------
execute_statement({assign, Name, Value}, Scope, Response) ->
    case execute_assign(Name, Value, Scope) of
        {ok, NewScope} -> {ok, NewScope, Response};
        Error          -> {error, {assign_failed, Error}}
    end;

execute_statement({call, Function, Arguments}, Scope, Response) ->
    case execute_call(Function, Arguments, Scope) of
        {ok, _} -> {ok, Scope, Response};
        Error   -> {error, {call_failed, Error}}
    end;

execute_statement({yield, Name}, Scope, Response) ->
    {ok, Scope, execute_yield(Name, Scope, Response)}.

%%--------------------------------------------------------------------
execute_assign(Name, {constant, Value}, Scope) ->
    {ok, dict:store(Name, Value, Scope)};

execute_assign(Name, {call, Function, Arguments}, Scope) ->
    case execute_call(Function, Arguments, Scope) of
        {ok, Value} -> {ok, dict:store(Name, Value, Scope)};
        Error       -> {error, {call_failed, Error}}
    end;

execute_assign(Name, {variable, VarName}, Scope) ->
    {ok, dict:store(Name, dict:fetch(VarName, Scope), Scope)}.

%%--------------------------------------------------------------------
execute_call(Function, Arguments, Scope) ->
    erlang:apply(graphbase_core_api, Function, [graphbase_dsl_utils:expand(Arguments, Scope)]).

%%--------------------------------------------------------------------
execute_yield(Name, Scope, Response) ->
    dict:store(Name, dict:fetch(Name, Scope), Response).
