-module(graphbase_core).

%% API exports
-export([
    with/3,
    get_confopt/1,
    get_confopt/2,
    get_confopt/3
]).

%%====================================================================
%% API functions
%%====================================================================

with(Initializer, Finalizer, Block) ->
    case Initializer() of
        {ok, Resource} ->
            try
                Block(Resource)
            of
                RetVal -> RetVal
            catch
                Exception:Reason ->
                    {error, {caught, Exception, Reason}}
            after
                case Finalizer(Resource) of
                    ok    -> ok;
                    Error -> throw({error, {resource_not_finalized, Error}})
                end
            end;
        
        Error ->
            {error, {resource_not_initialized, Error}}
    end.

%%--------------------------------------------------------------------
get_confopt(Option) ->
    get_confopt(Option, undefined).

%%--------------------------------------------------------------------
get_confopt(Option, Default) ->
    get_confopt(term, Option, Default).

%%--------------------------------------------------------------------
get_confopt(Type, Option, Default) ->
    Value = case os:getenv(atom_to_envvar(Option)) of
        false ->
            case application:get_env(Option) of
                {ok, StaticValue} -> parse_confval(Type, StaticValue);
                undefined         -> Default
            end;
        DynamicValue ->
                parse_confval(Type, DynamicValue)
    end,
    Value.

%%====================================================================
%% Internal functions
%%====================================================================

atom_to_envvar(Option) ->
    "GRAPHBASE_" ++ string:to_upper(atom_to_list(Option)).

%%--------------------------------------------------------------------
parse_confval(term, Value) ->
    {ok, Tokens, _} = erl_scan:string(Value),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term;

parse_confval(str, Value) ->
    Value;

parse_confval(int, Value) when is_integer(Value) ->
    Value;

parse_confval(int, Value) when is_list(Value) ->
    {Int, _} = string:to_integer(Value),
    Int;

parse_confval(float, Value) when is_float(Value) ->
    Value;

parse_confval(float, Value) when is_list(Value) ->
    string:to_float(Value).
