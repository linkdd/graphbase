%%%-------------------------------------------------------------------
%% @doc graphbase_dsl AST module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_ast).

%% API
-export([
    parse/1
]).

%%====================================================================
%% API functions
%%====================================================================

parse(Content) ->
    try
        {match, Tokens} = graphbase_dsl_lexer:lex(Content),
        walk(Tokens)
    of
        AST -> {ok, AST}
    catch
        E:R -> {error, {E, R}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

walk([{pipeline, StatementsTokens}]) ->
    walk(StatementsTokens);

walk([{statement, StatementTokens} | Tokens]) ->
    [Statement, {statement_end, _}] = StatementTokens,
    [walk([Statement]) | walk(Tokens)];

walk([{assign, AssignTokens}]) ->
    [
        {variable_name, VarName},
        {assign_operator, _},
        {expression, Expression}
    ] = AssignTokens,
    {assign, VarName, walk(Expression)};

walk([{yield, YieldTokens}]) ->
    [
        {yield, _},
        {left_parenthesis, _},
        {variable_name, VarName},
        {right_parenthesis, _}
    ] = YieldTokens,
    {yield, VarName};

walk([{function_call, CallTokens}]) ->
    [
        {identifier, Identifier},
        {left_parenthesis, _},
        {function_parameters, ParametersTokens},
        {right_parenthesis, _}
    ] = CallTokens,
    Parameters = walk(ParametersTokens),
    {call, to_identifier(Identifier), Parameters};

walk([{function_param, ParamTokens} | Tokens]) ->
    [
        {identifier, Identifier},
        {assign_operator, _},
        {expression, Expression}
    ] = ParamTokens,
    [{Identifier, walk(Expression)} | walk(Tokens)];

walk([{function_param_separator, _} | Tokens]) ->
    walk(Tokens);

walk([{identifier, Identifier}]) ->
    to_identifier(Identifier);

walk([{expression, ExpressionTokens}]) ->
    walk(ExpressionTokens);

walk([{variable_name, VarName}]) ->
    {variable, VarName};

walk([{constant, ConstantTokens}]) ->
    {constant, walk(ConstantTokens)};

walk([{boolean, [{Value, _}]}]) ->
    Value;

walk([{integer, [{digits, Digits}]}]) ->
    list_to_integer(binary_to_list(Digits));

walk([{integer, [{sign, Sign}, {digits, Digits}]}]) ->
    list_to_integer(binary_to_list(<<Sign/binary, Digits/binary>>));

walk([{decimal, [{integer, [{digits, Int}]}, {floating_point, FP}, {digits, Real}]}]) ->
    Decimal = <<Int/binary, FP/binary, Real/binary>>,
    {F, _} = string:to_float(binary_to_list(Decimal)),
    F;

walk([{decimal, [{integer, [{sign, Sign}, {digits, Int}]}, {floating_point, FP}, {digits, Real}]}]) ->
    Decimal = <<Sign/binary, Int/binary, FP/binary, Real/binary>>,
    {F, _} = string:to_float(binary_to_list(Decimal)),
    F;

walk([{string, [{data, String}]}]) ->
    string:trim(String, both, "\"");

walk([{list, Tokens}]) ->
    [{left_bracket, _} | TrailingTokens] = Tokens,
    [{right_bracket, _} | Elements] = lists:reverse(TrailingTokens),
    walk(lists:reverse(Elements));

walk([{tuple, Tokens}]) ->
    [{left_brace, _} | TrailingTokens] = Tokens,
    [{right_brace, _} | Elements] = lists:reverse(TrailingTokens),
    list_to_tuple(walk(lists:reverse(Elements)));

walk([{element, Element} | Tokens]) ->
    [walk(Element) | walk(Tokens)];

walk([{list_separator, _} | Tokens]) ->
    walk(Tokens);

walk([{tuple_separator, _} | Tokens]) ->
    walk(Tokens);

walk([]) ->
    [].

%%--------------------------------------------------------------------
to_identifier(Identifier) ->
    case lists:member(Identifier, keywords()) of
        true  -> list_to_atom(binary_to_list(Identifier));
        false ->
            T = fun(A) -> list_to_binary(atom_to_list(A)) end,
            Exports = [T(Name) || {Name, _} <- proplists:get_value(exports, graphbase_core_api:module_info())],
            ExportFilter = fun(Export) ->
                case Export of
                    module_info -> false;
                    _           -> true
                end
            end,
            Functions = lists:filter(ExportFilter, Exports),
            case lists:member(Identifier, Functions) of
                true  -> list_to_atom(binary_to_list(Identifier));
                false -> Identifier
            end
    end.

%%--------------------------------------------------------------------
keywords() ->
    [
        <<"empty">>,
        <<"property">>,
        <<"eq">>,
        <<"ne">>,
        <<"gt">>,
        <<"gte">>,
        <<"lt">>,
        <<"lte">>,
        <<"re">>,
        <<"match">>,
        <<"register">>,
        <<"flag">>,
        <<"counter">>,
        <<"map">>,
        <<"set">>
    ].
