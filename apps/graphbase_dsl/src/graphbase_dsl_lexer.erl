-module(graphbase_dsl_lexer).

%% API
-export([lex/1]).

%%====================================================================
%% API functions
%%====================================================================

lex(Code0) ->
    ETS = ets:new(memo, [set]),
    ets:insert(ETS, {code, Code0}),
    Res = case run_rule(pipeline(), ETS, string:trim(Code0)) of
        {match, Tokens, <<>>} -> {match, Tokens};
        {match, _, Code1}     -> {error, {unexpected_code, string:trim(Code1)}};
        {nomatch, Reason}     ->
            At = get_or_create_memo(at, ETS, 0),
            Stack = get_or_create_memo(stack, ETS, []),
            {error, {nomatch, [{at, At}, {stack, Stack}] ++ Reason}}
    end,
    ets:delete(ETS),
    Res.

%%====================================================================
%% Utility rules
%%====================================================================

named(Name, Rule) ->
    {Name, fun(ETS, Code0) ->
        case run_rule(Rule, ETS, Code0) of
            {match, Tokens, Code1} -> {match, [{Name, Tokens}], Code1};
            R                      -> R
        end
    end}.

zero_or_one(Rule) ->
    {zero_or_one, fun(ETS, Code) ->
        case run_rule(Rule, ETS, Code) of
            {nomatch, _} -> {match, [], Code};
            R            -> R
        end
    end}.

%%--------------------------------------------------------------------
zero_or_more(Rule) ->
    {zero_or_more, fun(ETS, Code0) ->
        case run_rule(Rule, ETS, Code0) of
            {nomatch, _}            -> {match, [], Code0};
            {match, Tokens0, Code1} ->
                R = zero_or_more(Rule),
                case run_rule(R, ETS, Code1) of
                    {match, [], Code2}      -> {match, Tokens0, Code2};
                    {match, Tokens1, Code3} -> {match, Tokens0 ++ Tokens1, Code3}
                end
        end
    end}.

%%--------------------------------------------------------------------
one_or_more(Rule) ->
    group(fun() -> [Rule, zero_or_more(Rule)] end).

%%--------------------------------------------------------------------
group(Rules) ->
    {group, fun(ETS, Code) -> group(ETS, Code, Rules(), []) end}.

%%--------------------------------------------------------------------
one_of(Rules) ->
    {one_of, fun(ETS, Code) -> one_of(ETS, Code, Rules(), []) end}.

%%--------------------------------------------------------------------
token(Name, String) ->
    {Name, fun(_ETS, Code0) ->
        case string:prefix(Code0, String) of
            nomatch -> {nomatch, [{expected, Name}]};
            Code1   -> {match, [{Name, String}], Code1}
        end
    end}.

%%--------------------------------------------------------------------
token_regex(Name, Regex) ->
    {Name, fun(_ETS, Code0) ->
        case re:run(Code0, "^" ++ Regex) of
            nomatch                 -> {nomatch, [{expected, Name}]};
            {match, [{Begin, End}]} ->
                Token = binary:part(Code0, Begin, End),
                Code1 = binary:part(Code0, End, byte_size(Code0) - End),
                {match, [{Name, Token}], Code1}
        end
    end}.

%%--------------------------------------------------------------------
whitespace() ->
    {whitespace, fun(ETS, Code0) ->
        R = token_regex(whitespace, "[\s\t\r\n]+"),
        case run_rule(R, ETS, Code0) of
            {nomatch, _}      -> {match, [], Code0};
            {match, _, Code1} -> {match, [], Code1}
        end
    end}.

%%====================================================================
%% Rules
%%====================================================================

pipeline() ->
    named(pipeline, one_or_more(statement())).

%%--------------------------------------------------------------------
statement() ->
    named(statement, group(fun() -> [
        whitespace(),
        one_of(fun() -> [
            assign(),
            yield(),
            function_call()
        ] end),
        whitespace(),
        token(statement_end, <<";">>)
    ] end)).

%%--------------------------------------------------------------------
assign() ->
    named(assign, group(fun() -> [
        whitespace(),
        variable_name(),
        whitespace(),
        token(assign_operator, <<"=">>),
        whitespace(),
        expression()
    ] end)).

%%--------------------------------------------------------------------
yield() ->
    named(yield, group(fun() -> [
        whitespace(),
        token(yield, <<"yield">>),
        whitespace(),
        left_parenthesis(),
        whitespace(),
        variable_name(),
        whitespace(),
        right_parenthesis()
    ] end)).

%%--------------------------------------------------------------------
function_call() ->
    named(function_call, group(fun() -> [
        whitespace(),
        identifier(),
        whitespace(),
        left_parenthesis(),
        whitespace(),
        named(function_parameters, zero_or_one(
            group(fun() -> [
                whitespace(),
                function_param(),
                whitespace(),
                zero_or_more(
                    group(fun() -> [
                        whitespace(),
                        token(function_param_separator, <<",">>),
                        whitespace(),
                        function_param()
                    ] end)
                )
            ] end)
        )),
        whitespace(),
        right_parenthesis()
    ] end)).

%%--------------------------------------------------------------------
function_param() ->
    named(function_param, group(fun() -> [
        whitespace(),
        identifier(),
        whitespace(),
        token(assign_operator, <<"=">>),
        whitespace(),
        expression()
    ] end)).

%%--------------------------------------------------------------------
identifier() ->
    token_regex(identifier, "[a-z][a-zA-Z0-9_]*").

%%--------------------------------------------------------------------
expression() ->
    named(expression, one_of(fun() -> [
        variable_name(),
        constant(),
        list(),
        tuple(),
        function_call()
    ] end)).

%%--------------------------------------------------------------------
variable_name() ->
    token_regex(variable_name, "[A-Z][a-zA-Z0-9_]*").

%%--------------------------------------------------------------------
constant() ->
    named(constant, one_of(fun() -> [
        boolean(),
        decimal(),
        integer(),
        string()
    ] end)).

%%--------------------------------------------------------------------
boolean() ->
    named(boolean, one_of(fun() -> [
        token(true, <<"true">>),
        token(false, <<"false">>)
    ] end)).

%%--------------------------------------------------------------------
integer() ->
    named(integer, group(fun() -> [
        zero_or_one(token(sign, <<"-">>)),
        whitespace(),
        digits()
    ] end)).

%%--------------------------------------------------------------------
decimal() ->
    named(decimal, group(fun() -> [
        whitespace(),
        integer(),
        token(floating_point, <<".">>),
        digits()
    ] end)).

%%--------------------------------------------------------------------
digits() ->
    token_regex(digits, "[0-9]+").

%%--------------------------------------------------------------------
string() ->
    named(string, group(fun() -> [
        whitespace(),
        token_regex(data, "\"(?:[^\"])*\"")
    ] end)).

%%--------------------------------------------------------------------
list() ->
    named(list, group(fun() -> [
        whitespace(),
        left_bracket(),
        whitespace(),
        zero_or_one(
            group(fun() -> [
                whitespace(),
                element(),
                whitespace(),
                zero_or_more(
                    group(fun() -> [
                        whitespace(),
                        token(list_separator, <<",">>),
                        whitespace(),
                        element()
                    ] end)
                )
            ] end)
        ),
        whitespace(),
        right_bracket()
    ] end)).

%%--------------------------------------------------------------------
tuple() ->
    named(tuple, group(fun() -> [
        whitespace(),
        left_brace(),
        whitespace(),
        zero_or_one(
            group(fun() -> [
                whitespace(),
                element(),
                whitespace(),
                zero_or_more(
                    group(fun() -> [
                        whitespace(),
                        token(tuple_separator, <<",">>),
                        whitespace(),
                        element()
                    ] end)
                )
            ] end)
        ),
        whitespace(),
        right_brace()
    ] end)).

%%--------------------------------------------------------------------
element() ->
    named(element, one_of(fun() -> [
        expression(),
        identifier()
    ] end)).

%%--------------------------------------------------------------------
list_comprehension() ->
    named(list_comprehension, group(fun() -> [
        whitespace(),
        token(comprehension_begin, <<"|">>),
        whitespace(),
        expression(),
        token(for, <<"for">>),
        whitespace(),
        variable_name(),
        whitespace(),
        token(in, <<"in">>),
        whitespace(),
        variable_name(),
        zero_or_one(group(fun() -> [
            whitespace(),
            token(condition, <<"if">>),
            whitespace(),
            expression()
        ] end)),
        whitespace(),
        token(comprehension_end, <<"|">>)
    ] end)).

%%--------------------------------------------------------------------
left_parenthesis() ->
    token(left_parenthesis, <<"(">>).

%%--------------------------------------------------------------------
right_parenthesis() ->
    token(right_parenthesis, <<")">>).

%%--------------------------------------------------------------------
left_bracket() ->
    token(left_bracket, <<"[">>).

%%--------------------------------------------------------------------
right_bracket() ->
    token(right_bracket, <<"]">>).

%%--------------------------------------------------------------------
left_brace() ->
    token(left_brace, <<"{">>).

%%--------------------------------------------------------------------
right_brace() ->
    token(right_brace, <<"}">>).

%%====================================================================
%% Internal functions
%%====================================================================

run_rule({RuleName, RuleFunction}, ETS, Code0) ->
    RuleStack0 = get_or_create_memo(stack, ETS, []),
    RuleStack1 = [{get_or_create_memo(at, ETS, 0), RuleName, string:slice(Code0, 0, 10)} | RuleStack0],
    update_memo(stack, ETS, RuleStack1),
    try
        RuleFunction(ETS, Code0)
    of
        {nomatch, R} ->
            {nomatch, R};
        
        {match, Tokens, Code1} ->
            OrigCode = get_or_create_memo(code, ETS, <<>>),
            update_memo(at, ETS, (byte_size(OrigCode) - byte_size(Code1))),
            update_memo(stack, ETS, RuleStack0),
            {match, Tokens, Code1}
    catch
        error:Reason ->
            {nomatch, Reason}
    end.

%%--------------------------------------------------------------------
group(ETS, Code0, [Rule | Rules], Acc) ->
    case run_rule(Rule, ETS, Code0) of
        {match, Tokens, Code1} -> group(ETS, Code1, Rules, Acc ++ Tokens);
        {nomatch, R}           -> {nomatch, R}
    end;

group(_ETS, Code, [], Acc) ->
    {match, Acc, Code}.

%%--------------------------------------------------------------------
one_of(ETS, Code, [Rule | Rules], Acc) ->
    case run_rule(Rule, ETS, Code) of
        {nomatch, E} -> one_of(ETS, Code, Rules, Acc ++ E);
        R            -> R
    end;
    
one_of(_ETS, _Code, [], Acc) ->
    {nomatch, [{unexpected, Acc}]}.

%%--------------------------------------------------------------------
get_or_create_memo(Memo, ETS, DefaultValue) ->
    case ets:lookup(ETS, Memo) of
        [{Memo, Value}] -> Value;
        []              -> update_memo(Memo, ETS, DefaultValue), DefaultValue
    end.

%%--------------------------------------------------------------------
update_memo(Memo, ETS, Value) ->
    ets:insert(ETS, {Memo, Value}).
