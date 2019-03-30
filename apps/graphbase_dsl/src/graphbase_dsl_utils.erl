%%%-------------------------------------------------------------------
%% @doc graphbase_dsl Utilities.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_utils).

%% API
-export([expand/2]).

%%====================================================================
%% API functions
%%====================================================================

expand(PropList, Scope) ->
    expand(PropList, Scope, []).

%%====================================================================
%% Internal functions
%%====================================================================

expand([{Key, {variable, Name}} | Properties], Scope, Acc) ->
    expand(Properties, Scope, [{Key, dict:fetch(Name, Scope)} | Acc]);

expand([{Key, {constant, Value}} | Properties], Scope, Acc) ->
    expand(Properties, Scope, [{Key, Value} | Acc]);

expand([], _Scope, Acc) ->
    Acc.
