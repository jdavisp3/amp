%% Reuse at will.

%% @private
-module(demo_handler).
-behaviour(amp_handler).

-export([init/1]).
-export([handle_ask/4]).
-export([handle_info/2]).
-export([terminate/2]).


init([]) ->
    error_logger:info_report(init),
    Sum = amp_command:new(
            <<"Sum">>,
            [{<<"a">>, integer, []},
             {<<"b">>, integer, []}],
            [{<<"total">>, integer, []}],
            [],
            [requires_answer]),
    Divide = amp_command:new(
               <<"Divide">>,
               [{<<"numerator">>, integer, []},
                {<<"denominator">>, integer, []}],
               [{<<"result">>, integer, []}],
               [{<<"ZERO_DIVISION">>, []}],
               [requires_answer]),
    {ok, 0, [Sum, Divide]}.

handle_ask(<<"Sum">>, Args, _, State) ->
    Sum = lists:sum([N || {_, N} <- Args]),
    {reply, {answer, Sum}, State + 1}.

handle_info(Msg, State) ->
    error_logger:info_report({info, Msg, State}),
    {ok, State}.

terminate(_, _) ->
    error_logger:info_report(terminate),
    ok.
