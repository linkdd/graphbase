%%%-------------------------------------------------------------------
%% @doc graphbase_dsl Utilities.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_utils).

%% API
-export([
    call/4
]).

%%====================================================================
%% API functions
%%====================================================================

call(User, Function, Arguments, Scope) ->
    erlang:apply(graphbase_core_api, Function, [User, expand(User, Arguments, Scope, [])]).

%%====================================================================
%% Internal functions
%%====================================================================

expand(User, [{Key, {variable, Name}} | Properties], Scope, Acc) ->
    expand(User, Properties, Scope, [{Key, dict:fetch(Name, Scope)} | Acc]);

expand(User, [{Key, {constant, Value}} | Properties], Scope, Acc) ->
    expand(User, Properties, Scope, [{Key, Value} | Acc]);

expand(User, [{Key, {call, Function, Arguments}} | Properties], Scope, Acc) ->
    case call(User, Function, Arguments, Scope) of
        {ok, Value} -> expand(User, Properties, Scope, [{Key, Value} | Acc]);
        Error       -> {error, {call_failed, Error}}
    end;

expand(_User, [], _Scope, Acc) ->
    Acc.
