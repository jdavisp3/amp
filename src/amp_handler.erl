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

-module(amp_handler).

-type opts() :: any().
-type state() :: any().
-type from() :: any().
-type reply() :: {answer, amp:amp_box()}
               | {error, Error::amp:amp_name(), Code::amp:amp_name(),
                  Description::amp:amp_name()}.
-type callback_opts() :: [hibernate | timeout()].
-type terminate_reason() :: {normal, shutdown}
                          | {normal, timeout}
                          | {error, closed}
                          | {error, badencoding}
                          | {error, badframe}
                          | {error, atom()}.

-callback init(opts()) ->
          {ok, state(), [amp:amp_command()]}
        | {ok, state(), [amp:amp_command()], callback_opts()}
        | shutdown.

-callback handle_ask(Name::binary(), Args::list(), from(), state()) ->
          {ok, state()}
        | {ok, state(), callback_opts()}
        | {reply, reply(), state()}
        | {reply, reply(), state(), callback_opts()}
        | {shutdown, state()}.

-callback handle_info(Msg::any(), state()) ->
          {ok, state()}
        | {ok, state(), callback_opts()}
        | {answer, from(), Response::amp:amp_box(), state()}
        | {answer, from(), Response::amp:amp_box(), state(), callback_opts()}
        | {error, from(), Error::amp:amp_name(), Code::amp:amp_name(),
           Description::amp:amp_name(), state()}
        | {error, from(), Error::amp:amp_name(), Code::amp:amp_name(),
           Description::amp:amp_name(), state(), callback_opts()}
        | {shutdown, state()}.

-callback terminate(terminate_reason(), state()) -> ok.
