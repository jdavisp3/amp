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
%%
%% The amp_command() type represents an AMP call/response
%% protocol. It is constructed from the following components:
%%
%%   name: the name of the amp command
%%   arguments: the valid list of arguments of the command
%%   response: the valid list responses to the command
%%   errors: the valid list of errors from the command
%%   options: list of protocol options
%%
%% The atom names of the AmpType enumeration have been chosen
%% to match the names used in the Twisted Python implementation
%% of Amp, with one addition (see below). This means that the
%% 'string' AmpType does not correspond to an Erlang string, but
%% a Python string.
%%
%% Amp strings are equivalent to Python strings in the Twisted
%% implementation of Amp and Python strings are really just byte
%% arrays. Erlang strings are lists of ascii character codes, while
%% binaries are used for arbitrary binary data (which could be a
%% string in some encoding). To deal with this mismatch, this library
%% provides two options.
%%
%% The 'string' AmpType will decode to an Erlang list of ascii
%% character codes. The 'binary' AmpType will decode to an Erlang
%% binary. Both types accept Erlang strings and binaries for encoding.
%%
%% The 'binary' type is also suitable for accepting other Amp types
%% not currently supported by the library. For example, by using the
%% binary type, Amp unicode values will be received as utf-8 encoded
%% Erlang binaries.

-module(amp_command).

-export([new/5]).
-export([name/1, arguments/1, response/1, errors/1, options/1]).

-export_type([amp_type/0, amp_list/0, amp_name/0,
              amp_option/0, amp_error/0, amp_error_option/0,
              amp_command/0]).

-type amp_type() :: 'string' | 'binary' | 'integer' | 'float' | 'boolean'
                  | {'amplist', amp_list()}.
-type amp_list() :: [{amp_name(), amp_type(), [amp_option()]}].
-type amp_name() :: binary().
-type amp_option() :: 'optional'.

-type amp_error() :: {amp_name(), [amp_error_option()]}.
-type amp_error_option() :: 'fatal'.

-type amp_command_option() :: 'requires_answer'.

-record(amp_command, {
          name :: amp_name(),
          arguments = [] :: [amp_type()],
          response = [] :: [amp_type()],
          errors = [] :: [amp_error()],
          options = [requires_answer] :: [amp_command_option()]
         }).

-opaque amp_command() :: #amp_command{}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


new(Name, Arguments, Response, Errors, Options) ->
    #amp_command{name=Name, arguments=Arguments, response=Response,
                 errors=Errors, options=Options}.

name(#amp_command{name=Name}) ->
    Name.

arguments(#amp_command{arguments=Arguments}) ->
    Arguments.

response(#amp_command{response=Response}) ->
    Response.

errors(#amp_command{errors=Errors}) ->
    Errors.

options(#amp_command{options=Options}) ->
    Options.


% Tests

-ifdef(TEST).

command_test() ->
    C = new(a, b, c, d, e),
    ?assertMatch(a, name(C)),
    ?assertMatch(b, arguments(C)),
    ?assertMatch(c, response(C)),
    ?assertMatch(d, errors(C)),
    ?assertMatch(e, options(C)).

-endif.
