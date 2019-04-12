-module(graphbase_dsl_lexer).

%% API
-export([lex/1]).

%% Utility rules
-export([
    zero_or_one/2,
    zero_or_more/2,
    one_or_more/2,
    group/2,
    one_of/2,
    token/3,
    token_regex/3,
    whitespace/1
]).

%% Rules
-export([
    pipeline/1,
    pipeline_separator/1,
    pipeline_end/1,
    statement/1,
    assign/1,
    yield/1,
    function_call/1,
    function_param/1,
    identifier/1,
    expression/1,
    variable_name/1,
    constant/1,
    boolean/1,
    integer/1,
    decimal/1,
    digits/1,
    string/1,
    list/1,
    tuple/1,
    element/1,
    left_parenthesis/1,
    right_parenthesis/1,
    left_bracket/1,
    right_bracket/1,
    left_brace/1,
    right_brace/1
]).

%%====================================================================
%% API functions
%%====================================================================

lex(Code0) ->
    R = rule(pipeline),
    case R(string:trim(Code0)) of
        {match, Tokens, ""} -> {match, Tokens};
        {nomatch, R}        -> {error, {nomatch, R}};
        {match, _, Code1}   -> {error, {unexpected_code, string:trim(Code1)}}
    end.

%%====================================================================
%% Utility rules
%%====================================================================

rule(Name) ->
    fun(Code) -> check_rule(Code, Name) end.

%%--------------------------------------------------------------------
rule(Name, Args) ->
    fun(Code) -> check_rule(Code, Name, Args) end.

%%--------------------------------------------------------------------
zero_or_one(Code, Rule) ->
    case Rule(Code) of
        {nomatch, _} -> {match, [], Code};
        R            -> R
    end.

%%--------------------------------------------------------------------
zero_or_more(Code0, Rule) ->
    case Rule(Code0) of
        {nomatch, _}            -> {match, [], Code0};
        {match, Tokens0, Code1} ->
            case zero_or_more(Code1, Rule) of
                {match, [], Code2}      -> {match, Tokens0, Code2};
                {match, Tokens1, Code3} -> {match, Tokens0 ++ Tokens1, Code3}
            end
    end.

%%--------------------------------------------------------------------
one_or_more(Code, Rule) ->
    group(Code, [Rule, rule(zero_or_more, [Rule])]).

%%--------------------------------------------------------------------
group(Code, Rules) ->
    group(Code, Rules, []).

%%--------------------------------------------------------------------
one_of(Code, [Rule | Rules]) ->
    case Rule(Code) of
        {nomatch, _} -> one_of(Code, Rules);
        R            -> R
    end;

one_of(_Code, []) ->
    {nomatch, unexpected_token}.

%%--------------------------------------------------------------------
token(Code0, Name, String) ->
    case string:prefix(Code0, String) of
        nomatch -> {nomatch, Name};
        Code1   -> {match, [{Name, String}], Code1}
    end.

%%--------------------------------------------------------------------
token_regex(Code, Name, Regex) ->
    case re:run(Code, "^" ++ Regex) of
        nomatch                 -> {nomatch, Name};
        {match, [{Begin, End}]} -> {
            match,
            [{Name, string:slice(Code, Begin, End)}],
            string:slice(Code, End)
        }
    end.

%%--------------------------------------------------------------------
whitespace(Code0) ->
    case token_regex(Code0, whitespace, "\s+") of
        {nomatch, whitespace} -> {match, [], Code0};
        {match, _, Code1}     -> {match, [], Code1}
    end.

%%====================================================================
%% Rules
%%====================================================================

pipeline(Code) ->
    R = rule(group, [[
        rule(whitespace),
        rule(statement),
        rule(whitespace),
        rule(zero_or_more, [
            rule(group, [[
                rule(whitespace),
                rule(pipeline_separator),
                rule(whitespace),
                rule(statement)
            ]])
        ]),
        rule(whitespace),
        rule(pipeline_end)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
pipeline_separator(Code) ->
    R = rule(token, [pipeline_separator, ","]),
    R(Code).

%%--------------------------------------------------------------------
pipeline_end(Code) ->
    R = rule(token, [pipeline_end, "."]),
    R(Code).

%%--------------------------------------------------------------------
statement(Code) ->
    R = rule(one_of, [[
        rule(assign),
        rule(yield),
        rule(function_call)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
assign(Code) ->
    R = rule(group, [[
        rule(whitespace),
        rule(variable_name),
        rule(whitespace),
        rule(token, [assign_operator, "="]),
        rule(whitespace),
        rule(expression)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
yield(Code) ->
    R = rule(group, [[
        rule(whitespace),
        rule(token, [yield, "yield"]),
        rule(whitespace),
        rule(left_parenthesis),
        rule(whitespace),
        rule(variable_name),
        rule(whitespace),
        rule(right_parenthesis)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
function_call(Code) ->
    R = rule(group, [[
        rule(whitespace),
        rule(identifier),
        rule(whitespace),
        rule(left_parenthesis),
        rule(whitespace),
        rule(zero_or_one, [
            rule(group, [[
                rule(whitespace),
                rule(function_param),
                rule(whitespace),
                rule(zero_or_more, [
                    rule(group, [[
                        rule(whitespace),
                        rule(token, [function_param_separator, ","]),
                        rule(whitespace),
                        rule(function_param)
                    ]])
                ])
            ]])
        ]),
        rule(whitespace),
        rule(right_parenthesis)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
function_param(Code) ->
    R = rule(group, [[
        rule(whitespace),
        rule(identifier),
        rule(whitespace),
        rule(token, [assign_operator, "="]),
        rule(whitespace),
        rule(expression)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
identifier(Code) ->
    R = rule(token_regex, [identifier, "[a-z][a-zA-Z0-9_]*"]),
    R(Code).

%%--------------------------------------------------------------------
expression(Code) ->
    R = rule(one_of, [[
        rule(variable_name),
        rule(constant),
        rule(function_call)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
variable_name(Code) ->
    R = rule(token_regex, [variable_name, "[A-Z][a-zA-Z0-9_]*"]),
    R(Code).

%%--------------------------------------------------------------------
constant(Code) ->
    R = rule(one_of, [[
        rule(boolean),
        rule(integer),
        rule(decimal),
        rule(string),
        rule(list),
        rule(tuple)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
boolean(Code) ->
    one_of(Code, [
        rule(token, [true, "true"]),
        rule(token, [false, "false"])
    ]).

%%--------------------------------------------------------------------
integer(Code) ->
    R = rule(group, [[
        rule(zero_or_one, [
            rule(token, [sign, "-"])
        ]),
        rule(whitespace),
        rule(digits)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
decimal(Code) ->
    R = rule(group, [[
        rule(whitespace),
        rule(integer),
        rule(token, [floating_point, "."]),
        rule(digits)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
digits(Code) ->
    R = rule(token_regex, [digits, "\d+"]),
    R(Code).

%%--------------------------------------------------------------------
string(Code) ->
    R = rule(group, [[
        rule(whitespace),
        rule(token, [string_begin, "\""]),
        rule(token_regex, [string_data, "\w*"]),
        rule(token, [string_end, "\""])
    ]]),
    R(Code).

%%--------------------------------------------------------------------
list(Code) ->
    R = rule(group, [[
        rule(whitespace),
        rule(left_bracket),
        rule(whitespace),
        rule(zero_or_one, [
            rule(group, [[
                rule(whitespace),
                rule(element),
                rule(whitespace),
                rule(zero_or_more, [
                    rule(group, [[
                        rule(whitespace),
                        rule(token, [list_separator, ","]),
                        rule(whitespace),
                        rule(element)
                    ]])
                ])
            ]])
        ]),
        rule(whitespace),
        rule(right_bracket)
    ]]),
    R(Code).


%%--------------------------------------------------------------------
tuple(Code) ->
    R = rule(group, [[
        rule(whitespace),
        rule(left_brace),
        rule(whitespace),
        rule(zero_or_one, [
            rule(group, [[
                rule(whitespace),
                rule(element),
                rule(whitespace),
                rule(zero_or_more, [
                    rule(group, [[
                        rule(whitespace),
                        rule(token, [tuple_separator, ","]),
                        rule(whitespace),
                        rule(element)
                    ]])
                ])
            ]])
        ]),
        rule(whitespace),
        rule(right_brace)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
element(Code) ->
    R = rule(one_of, [[
        rule(identifier),
        rule(variable_name),
        rule(constant),
        rule(function_call)
    ]]),
    R(Code).

%%--------------------------------------------------------------------
left_parenthesis(Code) ->
    R = rule(token, [left_parenthesis, "("]),
    R(Code).

%%--------------------------------------------------------------------
right_parenthesis(Code) ->
    R = rule(token, [right_parenthesis, ")"]),
    R(Code).

%%--------------------------------------------------------------------
left_bracket(Code) ->
    R = rule(token, [left_bracket, "["]),
    R(Code).

%%--------------------------------------------------------------------
right_bracket(Code) ->
    R = rule(token, [right_bracket, "]"]),
    R(Code).

%%--------------------------------------------------------------------
left_brace(Code) ->
    R = rule(token, [left_brace, "{"]),
    R(Code).

%%--------------------------------------------------------------------
right_brace(Code) ->
    R = rule(token, [right_brace, "}"]),
    R(Code).

%%====================================================================
%% Internal functions
%%====================================================================

check_rule(Code, Name) ->
    check_rule(Code, Name, []).

%%--------------------------------------------------------------------
check_rule(Code0, Name, Args) ->
    try
        erlang:apply(?MODULE, Name, [Code0 | Args])
    of
        {match, Tokens, Code1} -> {match, Tokens, Code1};
        {nomatch, R}           -> {nomatch, {Name, R}}
    catch
        error:{badmatch, R} -> {nomatch, {Name, R}}
    end.

%%--------------------------------------------------------------------
group(Code0, [Rule | Rules], Acc) ->
    case Rule(Code0) of
        {match, Tokens, Code1} -> group(Code1, Rules, Acc ++ Tokens);
        {nomatch, R}           -> {nomatch, R}
    end;

group(Code, [], Acc) ->
    {match, Acc, Code}.
