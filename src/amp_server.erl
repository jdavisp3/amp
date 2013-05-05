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
                commands=[] :: [amp:amp_command()],
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

-type ask_response() :: {amp_answer, amp:amp_box()}
                      | {amp_error, amp:amp_name(), amp:amp_box()}.


% @doc Send a question to the peer and get back the answer, an error
% from the command, or a general error.
-spec ask(pid(), amp_command:amp_name(), amp:amp_box()) -> ask_response().
ask(Pid, Name, Box) ->
    gen_server:call(Pid, {ask, Name, Box}).


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


handle_call({ask, Name, Box}, From, State) ->
    Command = lookup_command(State#state.commands, Name),
    State0 = ask_question(State, Command, Box, From),
    case amp_command:requires_answer(Command) of
        true ->
            {noreply, State0};
        false ->
            {reply, ok, State0}
    end;
handle_call(_, _From, State) ->
	{reply, ignored, State}.

% @private
handle_cast(_, State) ->
    {noreply, State}.

% @private
handle_info(_Info, State) ->
    {noreply, State}.

% @private
terminate(_Reason, _State) ->
    ok.

% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% @private
lookup_command([Command|Commands], Name) ->
    case amp_command:name(Command) of
        Name ->
            Command;
        _ ->
            lookup_command(Commands, Name)
    end.

% @private
% @doc Send a new question to the other side. Returns
% the Id of the new question and the new state of the server.
-spec ask_question(#state{}, amp:amp_command(), amp:box(), any()) -> #state{}.
ask_question(#state{socket=Socket, transport=Transport}=State,
             Command, Box, From) ->
    {Id, NextId} = make_id(State),
    Bin = amp_box:encode_ask(Command, Id, Box),
    Transport:send(Socket, Bin),
    Questions = dict:store(Id, From, State#state.questions),
    check_max_pending(Questions, State),
    NewState = State#state{nextid=NextId, questions=Questions},
    {Id, NewState}.

% @private
% @doc Given a state, return a new Id binary and a new nextid integer.
-spec make_id(#state{}) -> {binary(), integer()}.
make_id(#state{nextid=NextId}) ->
    Id = binary:list_to_bin(integer_to_list(NextId)),
    {Id, NextId + 1}.

% @private
check_max_pending(Dict, #state{max_pending=Max}) ->
    case dict:size(Dict) of
        Size when Size =< Max ->
            ok;
        _ ->
            error(max_pending_exceeded)
    end.
