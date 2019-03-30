%%%-------------------------------------------------------------------
%% @doc graphbase_dsl Interpreter module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_interpreter).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    stop/0,
    stop/1,
    run/1
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
run(AST) ->
    gen_server:call(?SERVER, {run, AST}).

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
    {ok, NewScope, NewResponse} = execute_statement(Statement, Scope, Response),
    execute(Statements, NewScope, NewResponse);

execute([], _Scope, Response) ->
    {ok, dict:to_list(Response)}.

%%--------------------------------------------------------------------
execute_statement({assign, Name, Value}, Scope, Response) ->
    {ok, execute_assign(Name, Value, Scope), Response};

execute_statement({call, Function, Arguments}, Scope, Response) ->
    execute_call(Function, Arguments, Scope),
    {ok, Scope, Response};

execute_statement({yield, Name}, Scope, Response) ->
    {ok, Scope, execute_yield(Name, Scope, Response)}.

%%--------------------------------------------------------------------
execute_assign(Name, {constant, Value}, Scope) ->
    dict:store(Name, Value, Scope);

execute_assign(Name, {call, Function, Arguments}, Scope) ->
    dict:store(Name, execute_call(Function, Arguments, Scope), Scope);

execute_assign(Name, {variable, VarName}, Scope) ->
    dict:store(Name, dict:fetch(VarName, Scope), Scope).

%%--------------------------------------------------------------------
execute_call(Function, Arguments, Scope) ->
    erlang:apply(graphbase_core_api, Function, graphbase_dsl_utils:expand(Arguments, Scope)).

%%--------------------------------------------------------------------
execute_yield(Name, Scope, Response) ->
    dict:store(Name, dict:fetch(Name, Scope), Response).
