%%%-------------------------------------------------------------------
%% @doc graphbase_dsl AST module.
%% @end
%%%-------------------------------------------------------------------

-module(graphbase_dsl_ast).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    stop/2,
    parse/2
]).

%% Generic server callbacks
-export([
    init/1,
    code_change/3,
    terminate/2,
    handle_info/2,
    handle_call/3,
    handle_cast/2
]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
stop(Parser) ->
    stop(Parser, normal).

%%--------------------------------------------------------------------
stop(Parser, Reason) ->
    gen_server:cast(Parser, {stop, Reason}).

%%--------------------------------------------------------------------
parse(Parser, Content) ->
    gen_server:call(Parser, {parse, Content}).

%%====================================================================
%% Generic Server callbacks
%%====================================================================

init(Args) ->
    Timeout = proplists:get_value(timeout, Args, 5000),
    {ok, nostate, Timeout}.

%%--------------------------------------------------------------------
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {stop, timeout, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_call({stop, Reason}, _From, State) ->
    {stop, Reason, ok, State};

handle_call({parse, Content}, _From, State) ->
    {reply, handle_parse(Content), State};

handle_call(Request, _From, State) ->
    {reply, {error, {invalid, Request}}, State}.

%%--------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    {stop, Reason, State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_parse(Content) ->
    {ok, Tokens, _} = erl_scan:string(Content),
    erl_parse:parse_term(Tokens).
