-module(graphbase_core_filter).

%% API exports
-export([match/2]).

%%====================================================================
%% API functions
%%====================================================================

match([Rule | Rules], PropList) ->
    case test(Rule, PropList) of
        true -> match(Rules, PropList);
        _    -> false
    end;

match([], _PropList) ->
    true.

%%====================================================================
%% Internal functions
%%====================================================================

test({empty, true}, PropList) ->
    case length(PropList) of
        0 -> true;
        _ -> false
    end;

test({empty, false}, PropList) ->
    case length(PropList) of
        0 -> false;
        _ -> true
    end;

test({property, Name, SubRule}, PropList) ->
    test_property(SubRule, proplists:get_value(Name, PropList)).

%%--------------------------------------------------------------------
test_property({eq, _Property}, _Property) ->
    true;
test_property({eq, _Value}, _Property) ->
    false;

test_property({ne, _Property}, _Property) ->
    false;
test_property({ne, _Value}, _Property) ->
    true;

test_property({gt, Value}, Property) when Property > Value ->
    true;
test_property({gt, _Value}, _Property) ->
    false;

test_property({gte, Value}, Property) when Property >= Value ->
    true;
test_property({gte, _Value}, _Property) ->
    false;

test_property({lt, Value}, Property) when Property < Value ->
    true;
test_property({lt, _Value}, _Property) ->
    false;

test_property({lte, Value}, Property) when Property =< Value ->
    true;
test_property({lte, _Value}, _Property) ->
    false;

test_property({re, RegEx}, Property) ->
    {ok, MP} = re:compile(RegEx),
    case re:run(Property, MP) of
        nomatch -> false;
        _       -> true
    end;

test_property({match, Rules}, Property) ->
    match(Rules, Property).
