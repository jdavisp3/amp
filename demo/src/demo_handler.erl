%% Reuse at will.

%% @private
-module(demo_handler).
-behaviour(amp_handler).

-export([init/1]).
-export([handle_ask/4]).
-export([handle_info/2]).
-export([terminate/2]).


init([]) ->
    {ok, undefined, []}.

handle_ask(Name, Args, From, State) ->
    ok.

handle_info(Msg, State) ->
    ok.

terminate(_, _) ->
    ok.
