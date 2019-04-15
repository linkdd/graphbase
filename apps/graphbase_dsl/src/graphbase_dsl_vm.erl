%%%-------------------------------------------------------------------
%% @doc graphbase_dsl Virtual Machine module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_vm).

%% API
-export([
    run/2
]).

-define(API, graphbase_dsl_api).

%%====================================================================
%% API functions
%%====================================================================

run(User, AST) ->
    run(User, AST, dict:new(), dict:new()).

%%====================================================================
%% Internal functions
%%====================================================================

run(User, [Statement | Statements], Scope, Response) ->
    case run_statement(User, Statement, Scope, Response) of
        {ok, NewScope, NewResponse} -> run(User, Statements, NewScope, NewResponse);
        {error, Reason}             -> {error, {statement_error, Reason}}
    end;

run(_User, [], _Scope, Response) ->
    {ok, dict:to_list(Response)}.

%%--------------------------------------------------------------------
run_statement(User, {assign, Name, Value}, Scope, Response) ->
    case run_assign(User, Name, Value, Scope) of
        {ok, NewScope}  -> {ok, NewScope, Response};
        {error, Reason} -> {error, {assign_failed, Name, Reason}}
    end;

run_statement(User, {call, Function, Arguments}, Scope, Response) ->
    case expand(User, {call, Function, Arguments}, Scope) of
        {ok, _}         -> {ok, Scope, Response};
        {error, Reason} -> {error, {call_failed, {Function, Arguments}, Reason}}
    end;

run_statement(_User, {yield, Name}, Scope, Response) ->
    {ok, Scope, run_yield(Name, Scope, Response)}.

%%--------------------------------------------------------------------
run_assign(User, Name, Expression, Scope) ->
    {ok, Value} = expand(User, Expression, Scope),
    {ok, dict:store(Name, Value, Scope)}.

%%--------------------------------------------------------------------
run_yield(Name, Scope, Response) ->
    dict:store(Name, dict:fetch(Name, Scope), Response).

%%--------------------------------------------------------------------
expand(_User, {variable, Name}, Scope) ->
    {ok, dict:fetch(Name, Scope)};

expand(User, {list, List}, Scope) ->
    {ok, [Value || {ok, Value} <- [expand(User, Element, Scope) || Element <- List]]};

expand(User, {tuple, Tuple}, Scope) ->
    {ok, Value} = expand(User, {list, tuple_to_list(Tuple)}, Scope),
    list_to_tuple(Value);

expand(_User, {constant, Value}, _Scope) ->
    {ok, Value};

expand(User, {proplist, PropList}, Scope) ->
    {ok, [{Key, Value} || {Key, {ok, Value}} <- [{K, expand(User, V, Scope)} || {K, V} <- PropList]]};

expand(User, {call, Function, Arguments}, Scope) ->
    {ok, Parameters} = expand(User, {proplist, Arguments}, Scope),
    erlang:apply(?API, Function, [User, Parameters]);

expand(_User, Value, _Scope) ->
    {ok, Value}.
