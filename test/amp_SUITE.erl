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

-module(amp_SUITE).

-include_lib("common_test/include/ct.hrl").

% common_test api
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

% tests
-export([success/1]).


all() ->
    [{group, amp}].

groups() ->
    Tests = [
             success
            ],
    [{amp, [parallel], Tests}].

init_per_suite(Config) ->
	application:start(ranch),
	Config.

end_per_suite(_Config) ->
	application:stop(ranch),
	ok.

init_per_group(amp, Config) ->
    {ok, Port, Name} = amp:listen([{handler, amp_test_handler}]),
    [{name, Name}, {port, Port} | Config].

end_per_group(_, Config) ->
    Name = proplists:get_value(name, Config),
    ranch:stop_listener(Name),
    ok.
