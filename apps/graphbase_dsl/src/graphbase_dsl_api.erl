%%%-------------------------------------------------------------------
%% @doc graphbase_dsl API module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_api).

-behaviour(gen_server).

%% API
-export([
    interpret/2
]).

%%====================================================================
%% API functions
%%====================================================================

interpret(User, Request) ->
    Reply = case graphbase_dsl_ast:parse(Request) of
        {ok, AST} ->
            case graphbase_dsl_interpreter:run(User, AST) of
                {ok, Response}  -> {ok, Response};
                {error, Reason} -> {error, {unable_to_interpret, Reason}}
            end;
        
        {error, Reason} ->
            {error, {unable_to_parse, Reason}}
    end,
    {reply, Reply, User}.
