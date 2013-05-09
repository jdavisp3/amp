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

-type decode_state() :: {header, binary()}
                      | {ask_header, binary()}
                      | {ask | answer | error, amp_box:decoder()}.

-record(state, {socket,
                transport,
                nextid = 0 :: non_neg_integer(),
                handler :: atom(),
                handler_state :: any(),
                timeout = infinity :: timeout(),
                timeout_ref = undefined :: undefined | reference(),
                commands = [] :: [amp:amp_command()],
                questions :: dict(), % id -> {From, Command} (pending questions we asked)
                answers :: dict(), % external id -> answer (pending answers we
                                   % have been asked),
                max_pending :: non_neg_integer(), % max # of pending q's & a's
                decode_state = {header, <<>>} :: decode_state()
               }).


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
    State0 = #state{socket=Socket, transport=Transport,
                    handler=proplists:get_value(handler, Opts),
                    questions=dict:new(), answers=dict:new(),
                    max_pending=proplists:get_value(max_pending, Opts, ?MAX_PENDING)},
    {State1, CallbackOpts} = init_handler(State0, Opts),
    State2 = update_timeout(State1, CallbackOpts),
    pre_loop(CallbackOpts, {gen_server, enter_loop, [?MODULE, [], State2]}).


% @private
init_handler(#state{handler=Handler, socket=Socket,
                    transport=Transport}=State, Opts) ->
    HandlerOpts = proplists:get_value(handler_opts, Opts, []),
    try Handler:init(HandlerOpts) of
        {ok, HandlerState, Commands} ->
            {State#state{handler_state=HandlerState, commands=Commands}, []};
        {ok, HandlerState, Commands, CallbackOpts} ->
            {State#state{handler_state=HandlerState, commands=Commands},
             CallbackOpts};
        shutdown ->
            Transport:close(Socket),
            exit(normal)
    catch Class:Reason ->
            error_logger:error_msg(
              "** Amp handler ~p terminating in ~p/~p~n"
              "   for the reason ~p:~p~n"
              "** Options were ~p~n"
              "** Stacktrace: ~p~n~n",
              [Handler, init, 1, Class, Reason,
               HandlerOpts, erlang:get_stacktrace()]),
            error(Reason)
    end.


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
handle_info({timeout, Ref, _}, #state{timeout_ref=Ref}=State) ->
    {stop, timeout, State#state{timeout=infinity, timeout_ref=undefined}};
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
    Questions = dict:store(Id, {From, Command}, State#state.questions),
    check_max_pending(Questions, State),
    State#state{nextid=NextId, questions=Questions}.

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

% @private
update_timeout(State, []) ->
    State;
update_timeout(#state{timeout_ref=PrevRef} = State,
               [{timeout, Timeout} | CallbackOpts]) ->
    case PrevRef of
        undefined ->
            ignore;
        PrevRef ->
            erlang:cancel_timer(PrevRef)
    end,
    Ref = case Timeout of
              infinity ->
                  undefined;
              Timeout ->
                  erlang:start_timer(Timeout, self(), ?MODULE)
          end,
    update_timeout(State#state{timeout=Timeout, timeout_ref=Ref}, CallbackOpts);
update_timeout(State, [_, CallbackOpts]) ->
    update_timeout(State, CallbackOpts).

% @private
pre_loop(CallbackOpts, {M, F, A}) ->
    case proplists:get_bool(hibernate, CallbackOpts) of
        true ->
                proc_lib:hibernate(M, F, A);
        false ->
            erlang:apply(M, F, A)
    end.
