%%%-------------------------------------------------------------------
%% @doc graphbase_apiserver Prometheus Metrics Exporter
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_apiserver_metrics).

%% API
-export([init/2]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req0 = #{method := <<"GET">>}, State) ->
    StatsByNode = [
        {Node, rpc:call(Node, graphbase_system_monitor, stats, [])} ||
        Node <- [node() | nodes()]
    ],
    Stats = lists:flatten([
        [{metric, Name, Value, [{node, atom_to_list(Node)} | Labels]} || {Name, Value, Labels} <- Metrics] ||
        {Node, Metrics} <- StatsByNode
    ]),
    Req = cowboy_req:reply(
        200,
        #{<<"content-type">> => "text/plain"},
        to_prometheus_format(Stats),
        Req0
    ),
    {ok, Req, State};

init(Req0, State) ->
    Req = cowboy_req:reply(
        405,
        #{<<"allow">> => <<"GET">>},
        Req0
    ),
    {ok, Req, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

to_prometheus_format(Metrics) ->
    to_prometheus_format(Metrics, <<>>).

to_prometheus_format([{metric, Name, Value, Labels} | Metrics], Response) ->
    BinName = list_to_binary(atom_to_list(Name)),
    BinValue = list_to_binary(io_lib:format("~p~n", [Value])),
    BinLabels = list_to_binary(lists:join(",", [
        atom_to_list(LabelName) ++ "=" ++ io_lib:format("~p", [LabelValue]) ||
        {LabelName, LabelValue} <- Labels
    ])),
    to_prometheus_format(Metrics, <<Response/binary, BinName/binary, "{", BinLabels/binary, "} ", BinValue/binary, "\n">>);

to_prometheus_format([], Response) ->
    Response.
