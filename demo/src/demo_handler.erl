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
    {ok, undefined, []}.

handle_ask(Name, Args, From, State) ->
    error_logger:info_report({ask, Name, Args, From, State}),
    {reply, yup, State}.

handle_info(Msg, State) ->
    error_logger:info_report({info, Msg, State}),
    {ok, State}.

terminate(_, _) ->
    error_logger:info_report(terminate),
    ok.
