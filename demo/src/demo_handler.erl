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
    {reply, sum(Args), State + 1};

handle_ask(<<"Divide">>, Args, _, State) ->
    Numerator = proplists:get_value(<<"numerator">>, Args),
    Denominator = proplists:get_value(<<"denominator">>, Args),
    {reply, divide(Numerator, Denominator), State + 1}.

handle_info(Msg, State) ->
    error_logger:info_report({info, Msg, State}),
    {ok, State}.

terminate(_, _) ->
    error_logger:info_report(terminate),
    ok.

sum([{_, A}, {_, B}]) ->
    {answer, [{<<"total">>, A + B}]}.

divide(_, 0) ->
    {error, <<"ZERO_DIVISION">>, <<"badness">>};
divide(N, D) ->
    {answer, [{<<"result">>, trunc(N / D)}]}.
