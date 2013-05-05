%% Copyright (c) 2013, Dave Peticolas <dave@krondo.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(amp_server).

-behaviour(ranch_protocol).
-behaviour(gen_server).

-record(state, {socket,
                transport,
                nextid=0,
                handler :: atom(),
                questions :: dict(), % id -> question (pending questions we asked)
                answers :: dict(), % external id -> answer (pending answers we
                                   % have been asked),
                max_pending :: non_neg_integer() % max # of pending q's & a's
               }).

%% AMP
-export([ask/3]).

%% ranch_protocol
-export([start_link/4]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MAX_PENDING, 1000).


%% @doc
%% Send a question to the peer. The return value includes a QuestionKey.
%% Once an answer, or a legal error, comes back from the peer, a message
%% will be sent to the calling process with the following form:
-spec ask(pid(), amp_command:amp_name(), KVPairs::list()) -> Result
                                              Proto = amp_command() | string()
                                              Result = {ok, QuestionKey}
                                              QuestionKey = {Pid, Id}
ask(Pid, Name, KVPairs) ->
    gen_server:call(Pid, {ask, Name, KVPairs}).


start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [[Ref, Socket, Transport, Opts]]).

init([Ref, Socket, Transport, Opts]) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    State = #state{socket=Socket, transport=Transport,
                   handler=proplists:get_value(handler, Opts),
                   questions=dict:new(), answers=dict:new(),
                   max_pending=proplists:get_value(max_pending, Opts, ?MAX_PENDING)},
    gen_server:enter_loop(?MODULE, [], State).
