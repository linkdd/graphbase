%%%-------------------------------------------------------------------
%% @doc graphbase_dsl Interpreter module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_interpreter).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    start_link/2,
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

start_link(User) ->
    start_link(User, []).

%%--------------------------------------------------------------------
start_link(User, Args) ->
    gen_server:start_link(?MODULE, {User, Args}, []).

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

init({User, Args}) ->
    Timeout = proplists:get_value(timeout, Args, 5000),
    {ok, User, Timeout}.

%%--------------------------------------------------------------------
code_change(_OldVersion, User, _Extra) ->
    {ok, User}.

%%--------------------------------------------------------------------
terminate(_Reason, _User) ->
    ok.

%%--------------------------------------------------------------------
handle_info(timeout, User) ->
    {stop, timeout, User};

handle_info(_Info, User) ->
    {noreply, User}.

%%--------------------------------------------------------------------
handle_call({stop, Reason}, _From, User) ->
    {stop, Reason, ok, User};

handle_call({run, AST}, _From, User) ->
    {reply, execute(User, AST), User};

handle_call(Request, _From, User) ->
    {reply, {error, {invalid, Request}}, User}.

%%--------------------------------------------------------------------
handle_cast({stop, Reason}, User) ->
    {stop, Reason, User}.

%%====================================================================
%% Internal functions
%%====================================================================

execute(User, AST) ->
    execute(User, AST, dict:new(), dict:new()).

%%--------------------------------------------------------------------
execute(User, [Statement | Statements], Scope, Response) ->
    case execute_statement(User, Statement, Scope, Response) of
        {ok, NewScope, NewResponse} ->
            execute(User, Statements, NewScope, NewResponse);
        Error ->
            {error, {statement_error, Error}}
    end;

execute(_User, [], _Scope, Response) ->
    {ok, dict:to_list(Response)}.

%%--------------------------------------------------------------------
execute_statement(User, {assign, Name, Value}, Scope, Response) ->
    case execute_assign(User, Name, Value, Scope) of
        {ok, NewScope} -> {ok, NewScope, Response};
        Error          -> {error, {assign_failed, Error}}
    end;

execute_statement(User, {call, Function, Arguments}, Scope, Response) ->
    case graphbase_dsl_utils:call(User, Function, Arguments, Scope) of
        {ok, _} -> {ok, Scope, Response};
        Error   -> {error, {call_failed, Error}}
    end;

execute_statement(_User, {yield, Name}, Scope, Response) ->
    {ok, Scope, execute_yield(Name, Scope, Response)}.

%%--------------------------------------------------------------------
execute_assign(_User, Name, {constant, Value}, Scope) ->
    {ok, dict:store(Name, Value, Scope)};

execute_assign(User, Name, {call, Function, Arguments}, Scope) ->
    case graphbase_dsl_utils:call(User, Function, Arguments, Scope) of
        {ok, Value} -> {ok, dict:store(Name, Value, Scope)};
        Error       -> {error, {call_failed, Error}}
    end;

execute_assign(_User, Name, {variable, VarName}, Scope) ->
    {ok, dict:store(Name, dict:fetch(VarName, Scope), Scope)}.

%%--------------------------------------------------------------------
execute_yield(Name, Scope, Response) ->
    dict:store(Name, dict:fetch(Name, Scope), Response).
