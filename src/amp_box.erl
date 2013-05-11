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
%% @doc
%% AMP protocol utilities. See:
%%  [http://twistedmatrix.com/documents/current/api/twisted.protocols.amp.html]
%%  [http://twistedmatrix.com/documents/current/core/howto/amp.html]
%%
%% This module implements low-level encoding and decoding of AMP
%% Boxes. An Amp Box is represented in Erlang as a list of key/value
%% pairs.

-module(amp_box).

%% API
-export([encode_ask/3, encode_answer/3, encode_error/4,
         new_decoder/0, decode_bin_box/2, identify_bin_box/1,
         decode_box/2]).

-export_type([decoder/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(decoder, {
          box = [] :: amp:amp_bin_box(),
          rest = <<>> :: binary()
         }).

-opaque decoder() :: #decoder{}.

% Pre-defined key names
-define(AMP_KEY_ASK, <<"_ask">>).
-define(AMP_KEY_COMMAND, <<"_command">>).
-define(AMP_KEY_ANSWER, <<"_answer">>).
-define(AMP_KEY_ERROR, <<"_error">>).
-define(AMP_KEY_ERROR_CODE, <<"_error_code">>).
-define(AMP_KEY_ERROR_DESCRIPTION, <<"_error_description">>).

% Limits
-define(AMP_MAX_KEY_LEN, 255).
-define(AMP_MAX_VAL_LEN, 65535).

% The protocol for error responses.
-define(AMP_ERROR_PROTOCOL, [{?AMP_KEY_ERROR_CODE, string, []},
                             {?AMP_KEY_ERROR_DESCRIPTION, string, []}]).


%% @doc Given an amp_command record, a message id, and a box, return an
%% iodata encoding of the Amp Box that would implement the call.
-spec encode_ask(amp_command:amp_command(), binary(), amp:amp_box()) -> iodata().
encode_ask(Command, Id, Box) ->
    encode_box([{?AMP_KEY_ASK, string, []},
                {?AMP_KEY_COMMAND, string, []}
                | amp_command:arguments(Command)],
               [{?AMP_KEY_ASK, Id},
                {?AMP_KEY_COMMAND, amp_command:name(Command)}
                | Box]).


%% @doc Given an amp_command record, a message id, and a box, return an
%% iodata encoding of the AmpBox that would implement the answer box for
%% a call.
-spec encode_answer(amp_command:amp_command(), binary(),
                    amp:amp_box()) -> iodata().
encode_answer(Command, Id, Box) ->
    encode_box([{?AMP_KEY_ANSWER, string, []} | amp_command:response(Command)],
               [{?AMP_KEY_ANSWER, Id} | Box]).

%% @doc Given an amp_command record, a message id, and an error code,
%% and a description, return an iodata encoding of the Amp Box that
%% would implement the error box for a call.
-spec encode_error(amp_command:amp_command(), binary(),
                   binary(), binary()) -> iodata().
encode_error(Command, Id, ErrorCode, Description) ->
    {value, _} = lists:keysearch(ErrorCode, 1, amp_command:errors(Command)),
    encode_box([{?AMP_KEY_ERROR, string, []} | ?AMP_ERROR_PROTOCOL],
               [{?AMP_KEY_ERROR, Id},
                {?AMP_KEY_ERROR_CODE, ErrorCode},
                {?AMP_KEY_ERROR_DESCRIPTION, Description}]).


%% @doc Return a new decoder object suitable for unserializing a wire
%% format of an Amp box. Decoders are required arguments for
%% decode_bin_box/2.
-spec new_decoder() -> #decoder{}.
new_decoder() ->
    #decoder{}.

% @doc Decode a part (or whole) of a box.
% If the complete box is decoded, return the box we decoded and a
% new decoder state. The decoder may contain another box, so
% a second decode call with an empty binary is needed.
-spec decode_bin_box(decoder(), binary()) ->
                            {not_done, decoder()} |
                            {amp:amp_bin_box(), decoder()}.
decode_bin_box(#decoder{rest=Old}=Decoder, New) ->
    case decode_bin_box_int(Decoder#decoder.box, <<Old/binary, New/binary>>) of
        {not_done, Box, Rest} ->
            {not_done, Decoder#decoder{box=Box, rest=Rest}};
        {Box, Rest} ->
            {lists:reverse(Box), #decoder{rest=Rest}}
    end.

-spec identify_bin_box(amp:amp_bin_box()) ->
                              {ask, Id::binary(), Name::binary(), amp:amp_bin_box()}
                                  | {answer, Id::binary(), amp:amp_bin_box()}
                                  | {error, Id::binary(), amp:amp_bin_box()}.
identify_bin_box(Box) ->
    identify_bin_box(Box, undefined, undefined, undefined, []).

identify_bin_box([], ask, _, undefined, _) ->
    error(illegal_box);
identify_bin_box([], ask, Id, Name, Box) ->
    {ask, Id, Name, Box};
identify_bin_box([], answer, Id, undefined, Box) ->
    {answer, Id, Box};
identify_bin_box([], error, Id, undefined, Box) ->
    {error, Id, Box};
identify_bin_box([{?AMP_KEY_ASK, Id}|Rest], undefined, undefined, Name, Box) ->
    identify_bin_box(Rest, ask, Id, Name, Box);
identify_bin_box([{?AMP_KEY_ASK, _}|_], _, _, _, _) ->
    error(illegal_box);
identify_bin_box([{?AMP_KEY_ANSWER, Id}|Rest], undefined, undefined, undefined, Box) ->
    identify_bin_box(Rest, answer, Id, undefined, Box);
identify_bin_box([{?AMP_KEY_ANSWER, _}|_], _, _, _, _) ->
    error(illegal_box);
identify_bin_box([{?AMP_KEY_ERROR, Id}|Rest], undefined, undefined, undefined, Box) ->
    identify_bin_box(Rest, error, Id, undefined, Box);
identify_bin_box([{?AMP_KEY_ERROR, _}|_], _, _, _, _) ->
    error(illegal_box).


-spec decode_box(amp:amp_list(), amp:amp_bin_box()) -> amp:amp_box().
decode_box(Protocol, Box) ->
    ok.

% @private
% @doc Encode the box according to the given protocol into iodata.
-spec encode_box(amp_command:amp_list(), amp:amp_box()) -> iodata().
encode_box([], []) ->
    [<<0, 0>>];
encode_box([{Key, Type, Options} | Protocol], Box) ->
    case lists:keytake(Key, 1, Box) of
        {value, {Key, Value}, Box2} ->
            <<_, _/binary>> = Key, % no empty keys
            EncKeyLength = encode_length(size(Key), ?AMP_MAX_KEY_LEN),
            EncValue = encode_value(Value, Type),
            EncLength = encode_length(iolist_size(EncValue), ?AMP_MAX_VAL_LEN),
            [EncKeyLength, Key, EncLength, EncValue | encode_box(Protocol, Box2)];
        false ->
            true = proplists:get_bool(optional, Options),
            encode_box(Protocol, Box)
    end.

% @private
% @doc Decode a binary box as much as possible and return the results.
-spec decode_bin_box_int(amp:amp_bin_box(), binary()) ->
                                {not_done, amp:amp_bin_box(), binary()}
                                    | {amp:amp_bin_box(), binary()}.
decode_bin_box_int(Box, Bin) when size(Bin) < 2 ->
    {not_done, Box, Bin};
decode_bin_box_int(Box, <<0, 0, Rest/binary>>) ->
    {Box, Rest};
decode_bin_box_int(Box, Bin) ->
    case match_kvp(Bin) of
        not_done ->
            {not_done, Box, Bin};
        {Key, ValBin, Rest} ->
            decode_bin_box_int([{Key, ValBin} | Box], Rest)
    end.

% @private
% Match a key/value pair encoded at the front of the binary.  Return a
% tuple with the key name, the value still encoded as a binary, and
% the remaining bytes that were not used in the kvp. Or, if there are
% not enough bytes to decode the first kvp, return not_done.
-spec match_kvp(binary()) -> {Key::amp_command:amp_name(),
                              ValBin::binary(),
                              Rest::binary()} | 'not_done'.
match_kvp(<<0, KeyLen, Key:KeyLen/binary,
            ValLen:16/unsigned-big, Val:ValLen/binary, Rest/binary>>) ->
    {Key, Val, Rest};
match_kvp(<<0, _/binary>>) ->
    not_done;
match_kvp(<<>>) ->
    not_done.


% @private
% @doc Encode a length, given the maximum value of that length.
-spec encode_length(non_neg_integer(), non_neg_integer()) -> binary().
encode_length(Length, Max) when Length =< Max ->
    <<Length:16/unsigned-big>>.


% @private
% @doc Encode a value given its type into an iolist.
-spec encode_value(term(), amp_command:amp_type()) -> iolist().
encode_value(Value, integer) when is_integer(Value) ->
    integer_to_list(Value);
encode_value(Value, float) when is_float(Value) ->
    float_to_list(Value);
encode_value(Value, float) when is_integer(Value) ->
    float_to_list(float(Value));
encode_value(Value, boolean) when is_boolean(Value) ->
    case Value of
        true -> <<"True">>;
        false -> <<"False">>
    end;
encode_value(Value, string) when is_list(Value) ; is_binary(Value) ->
    Value;
encode_value(Value, binary) when is_list(Value) ; is_binary(Value) ->
    Value;
encode_value(Value, {amplist, Protocol}) ->
    [encode_box(Protocol, Box) || Box <- Value].


% @private
% @doc Decode a value in binary form given its type.
-spec decode_value(binary(), amp_command:amp_type()) -> term().
decode_value(ValBin, integer) ->
    erlang:list_to_integer(erlang:binary_to_list(ValBin));
decode_value(ValBin, float) ->
    Val = erlang:binary_to_list(ValBin),
    erlang:list_to_float(ensure_decimal(Val));
decode_value(<<"True">>, boolean) ->
    true;
decode_value(<<"False">>, boolean) ->
    false;
decode_value(ValBin, string) ->
    erlang:binary_to_list(ValBin);
decode_value(ValBin, binary) ->
    ValBin;
decode_value(ValBin, {amplist, Protocol}) ->
    decode_amplist(ValBin, Protocol).


% @private
% @doc Decode an amplist value and return the list of boxes.
-spec decode_amplist(binary(), amp_command:amp_list()) -> [amp:amp_box()].
decode_amplist(<<>>, _Protocol) ->
    [];
decode_amplist(Bin, Protocol) ->
    {done, KVPairs, Rest} = decode_bin_box([], Bin),
    [lists:reverse(KVPairs) | decode_amplist(Rest, Protocol)].


% @private
% @doc Return the original string, if it has a decimal point,
% or the original string followed by .0 if not.
-spec ensure_decimal(string()) -> string().
ensure_decimal(String) ->
    case lists:member($., String) of
        true ->
            String;
        false ->
            String ++ ".0"
    end.


% @private
% @doc Return a box type atom given the binary box.
-spec box_type(amp:amp_bin_box()) -> ask | answer | error.
box_type([{?AMP_KEY_ASK, _}|_]) ->
    ask;
box_type([{?AMP_KEY_ANSWER, _}|_]) ->
    answer;
box_type([{?AMP_KEY_ERROR, _}|_]) ->
    error;
box_type([_|Box]) ->
    box_type(Box).


% Tests

-ifdef(TEST).

encode_test_() ->
    Encode = fun (P, B) -> iolist_to_binary(encode_box(P, B)) end,
    [
     ?_assertMatch(<<0, 1, $a, 0, 1, $1, 0, 0>>,
                   Encode([{<<"a">>, integer, []}], [{<<"a">>, 1}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   Encode([{<<"aa">>, string, []}], [{<<"aa">>, "xyz"}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   Encode([{<<"aa">>, string, []}], [{<<"aa">>, <<"xyz">>}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   Encode([{<<"aa">>, string, []}], [{<<"aa">>, "xyz"}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   Encode([{<<"aa">>, binary, []}], [{<<"aa">>, <<"xyz">>}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   Encode([{<<"aa">>, binary, []}], [{<<"aa">>, "xyz"}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   Encode([{<<"aa">>, binary, []}], [{<<"aa">>, <<"xyz">>}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 26, "1.50000000000000000000e+00", 0, 0>>,
                   Encode([{<<"aa">>, float, []}], [{<<"aa">>, 1.5}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 26, "1.00000000000000000000e+00", 0, 0>>,
                   Encode([{<<"aa">>, float, []}], [{<<"aa">>, 1}])),
     ?_assertMatch(<<0, 1, $a, 0, 4, "True", 0, 0>>,
                   Encode([{<<"a">>, boolean, []}], [{<<"a">>, true}])),
     ?_assertMatch(<<0, 1, $a, 0, 5, "False", 0, 0>>,
                   Encode([{<<"a">>, boolean, []}], [{<<"a">>, false}])),
     ?_assertMatch(<<0, 1, $a, 0, 1, $1, 0, 0>>,
                   Encode([{<<"b">>, integer, [optional]},
                           {<<"a">>, integer, []}],
                          [{<<"a">>, 1}])),
     ?_assertMatch(<<0, 1, $b, 0, 1, $2, 0, 1, $a, 0, 1, $1, 0, 0>>,
                   Encode([{<<"b">>, integer, [optional]}, {<<"a">>, integer, []}],
                          [{<<"a">>, 1}, {<<"b">>, 2}])),
     ?_assertMatch(<<0, 1, $a, 0, 8, 0, 1, $b, 0, 1, $5, 0, 0, 0, 0>>,
                   Encode([{<<"a">>, {amplist, [{<<"b">>, integer, []}]}, []}],
                          [{<<"a">>, [[{<<"b">>, 5}]]}])),
     ?_assertMatch(<<0, 1, $a, 0, 16, 0, 1, $b, 0, 1, $5, 0, 0,
                                      0, 1, $b, 0, 1, $0, 0, 0, 0, 0>>,
                   Encode([{<<"a">>, {amplist, [{<<"b">>, integer, []}]}, []}],
                          [{<<"a">>, [[{<<"b">>, 5}], [{<<"b">>, 0}]]}])),
     ?_assertError(_, encode_box([{[256], string, []}], [{[256], "xyz"}])),
     ?_assertError(_, encode_box([{<<"aa">>, string, []}], [{<<"aa">>, 1}])),
     ?_assertError(_, encode_box([{<<"">>, string, []}], [{<<"">>, "a"}])),
     ?_assertError(_, encode_box([{<<"aa">>, float, []}], [{<<"aa">>, "apple"}])),
     ?_assertError(_, encode_box([{<<"b">>, integer, [optional]}], [{<<"a">>, 1}]))
    ].

encode_ask_test() ->
    Cmd = amp_command:new(<<"n">>, [{<<"a">>, string, []}], nil, nil, []),
    Bin = iolist_to_binary(encode_ask(Cmd, <<"1">>, [{<<"a">>, "A"}])),
    ?assertMatch(Bin, <<0, 4, "_ask", 0, 1, "1",
                        0, 8, "_command", 0, 1, "n",
                        0, 1, "a", 0, 1, "A", 0, 0>>),
    Decoder = new_decoder(),
    ?assertMatch({[{<<"_ask">>,<<"1">>},
                   {<<"_command">>,<<"n">>},
                   {<<"a">>,<<"A">>}], Decoder},
                 decode_bin_box(Decoder, Bin)).

encode_answer_test() ->
    Cmd = amp_command:new(<<"n">>, nil,
                          [{<<"b">>, string, []}], [], []),
    Bin = iolist_to_binary(encode_answer(Cmd, <<"1">>, [{<<"b">>, "B"}])),
    ?assertMatch(Bin, <<0, 7, "_answer", 0, 1, "1",
                        0, 1, "b", 0, 1, "B", 0, 0>>),
    Decoder = new_decoder(),
    ?assertMatch({[{<<"_answer">>, <<"1">>},
                   {<<"b">>, <<"B">>}], Decoder},
                 decode_bin_box(Decoder, Bin)).

encode_error_test() ->
    Cmd = amp_command:new(<<"n">>, nil, nil, 
                          [{<<"A">>, []},
                           {<<"B">>, [fatal]}], []),
    Bin1 = iolist_to_binary(encode_error(Cmd, <<"1">>, <<"A">>, <<"AA">>)),
    ?assertMatch(Bin1, <<0, 6, "_error", 0, 1, "1",
                         0, 11, "_error_code", 0, 1, "A",
                         0, 18, "_error_description", 0, 2, "AA", 0, 0>>),
    Decoder = new_decoder(),
    ?assertMatch({[{<<"_error">>, <<"1">>},
                   {<<"_error_code">>, <<"A">>},
                   {<<"_error_description">>, <<"AA">>}], Decoder},
                 decode_bin_box(Decoder, Bin1)),

    Bin2 = iolist_to_binary(encode_error(Cmd, "2", <<"B">>, <<"BB">>)),
    ?assertMatch({[{<<"_error">>, <<"2">>},
                   {<<"_error_code">>, <<"B">>},
                   {<<"_error_description">>, <<"BB">>}], Decoder},
                 decode_bin_box(Decoder, Bin2)).


test_one_by_one(Decoder, <<Byte, Input/binary>>) ->
    case decode_bin_box(Decoder, <<Byte>>) of
        {not_done, Decoder1} ->
            test_one_by_one(Decoder1, Input);
        Result ->
            {Result, Input}
    end.

decode_1_test() ->
    Decoder1 = new_decoder(),
    {not_done, Decoder2} = decode_bin_box(Decoder1, <<0>>),
    ?assert(Decoder2#decoder.box == []),
    {not_done, Decoder3} = decode_bin_box(Decoder2, <<1, $a>>),
    ?assert(Decoder3#decoder.rest == <<0, 1, $a>>),
    ?assert(Decoder3#decoder.box == []).

decode_2_test() ->
    Decoder = new_decoder(),
    Input = <<0, 1, $a, 0, 1, $1, 0, 0>>,
    ?assertMatch({[{<<"a">>, <<"1">>}], Decoder},
                 decode_bin_box(Decoder, Input)).

decode_3_test() ->
    Decoder = new_decoder(),
    Input = <<0, 1, $a, 0, 1, $4, 0, 0, 14>>,
    ?assertMatch({[{<<"a">>, <<"4">>}], #decoder{rest = <<14>>}},
                 decode_bin_box(Decoder, Input)).

decode_4_test() ->
    Decoder = new_decoder(),
    Input1 = <<0, 7, "billy o", 0, 5, "12345",
               0, 4, "name", 0, 5, "nimbo",
               0, 0>>,
    ?assertMatch({{[{<<"billy o">>, <<"12345">>},
                    {<<"name">>, <<"nimbo">>}], Decoder}, <<>>},
                 test_one_by_one(Decoder, Input1)),
    Input2 = <<0, 4, "name", 0, 5, "nimbo",
               0, 7, "billy o", 0, 5, "12345",
               0, 0>>,
    ?assertMatch({{[{<<"name">>, <<"nimbo">>},
                    {<<"billy o">>, <<"12345">>}], Decoder}, <<>>},
                 test_one_by_one(Decoder, Input2)).

decode_5_test() ->
    Decoder = new_decoder(),
    Input = <<1, 4, "name", 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_6_test() ->
    Decoder = new_decoder(),
    Input = <<0, 4, "namx", 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_7_test() ->
    Decoder = new_decoder(),
    Input = <<0, 1, "q", 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_8_test() ->
    Decoder = new_decoder(),
    Input = <<0, 1, $;, 0, 3, "1.5", 0, 0, 1, 2, 3>>,
    ?assertMatch({[{<<";">>, <<"1.5">>}],
                  #decoder{rest = <<1, 2, 3>>}},
                 decode_bin_box(Decoder, Input)),
    Input2 = <<0, 1, $;, 0, 1, "1", 0, 0, 1, 2, 3>>,
    ?assertMatch({[{<<";">>, <<"1">>}],
                  #decoder{rest = <<1, 2, 3>>}},
                 decode_bin_box(Decoder, Input2)).

decode_9_test() ->
    Decoder = new_decoder(),
    Input1 = <<0, 2, "//", 0, 4, "True", 0, 0>>,
    ?assertMatch({[{<<"//">>, <<"True">>}], Decoder},
                 decode_bin_box(Decoder, Input1)),
    Input2 = <<0, 2, "//", 0, 5, "False", 0, 0>>,
    ?assertMatch({[{<<"//">>, <<"False">>}], Decoder},
                 decode_bin_box(Decoder, Input2)).

decode_10_test() ->
    Decoder = new_decoder(),
    Input1 = <<0, 1, $a, 0, 1, $4, 0, 1, $b, 0, 1, $5, 0, 0>>,
    ?assertMatch({[{<<"a">>, <<"4">>}, {<<"b">>, <<"5">>}], Decoder},
                 decode_bin_box(Decoder, Input1)),
    Input2 = <<0, 1, $b, 0, 1, $5, 0, 0>>,
    ?assertMatch({[{<<"b">>, <<"5">>}], Decoder},
                 decode_bin_box(Decoder, Input2)).

decode_11_test() ->
    Decoder = new_decoder(),
    Input1 = <<0, 1, $a, 0, 1, $4, 0, 1, $b, 0, 1, $5, 0, 0>>,
    ?assertMatch({[{<<"a">>, <<"4">>}, {<<"b">>, <<"5">>}], Decoder},
                 decode_bin_box(Decoder, Input1)),
    Input2 = <<0, 1, $a, 0, 1, $5, 0, 0>>,
    ?assertMatch({[{<<"a">>, <<"5">>}], Decoder},
                 decode_bin_box(Decoder, Input2)).

decode_12_test() ->
    Decoder = new_decoder(),

    Input1 = <<0, 1, $', 0, 1, $0, 0, 0, 1, 2, 3>>,
    ?assertMatch({[{<<"'">>, <<"0">>}], #decoder{rest = <<1, 2, 3>>}},
                 decode_bin_box(Decoder, Input1)),

    Input2 = <<0, 1, $', 0, 1, $0,
              0, 1, $s, 0, 14,
                0, 1, $h, 0, 1, $1, 0, 1, $g, 0, 1, $2, 0, 0,
              0, 0, 1, 2, 3>>,
    ?assertMatch({[{<<"'">>, <<"0">>},
                   {<<"s">>, <<0, 1, $h, 0, 1, $1, 0, 1, $g, 0, 1, $2, 0, 0>>}],
                  #decoder{rest = <<1, 2, 3>>}},
                 decode_bin_box(Decoder, Input2)),

    Input3 = <<0, 1, $', 0, 1, $0,
              0, 1, $s, 0, 22,
                0, 1, $h, 0, 1, $1, 0, 1, $g, 0, 1, $2, 0, 0,
                0, 1, $h, 0, 1, $5, 0, 0,
              0, 0, 1, 2, 3>>,
    ?assertMatch({[{<<"'">>, <<"0">>},
                   {<<"s">>, <<0, 1, $h, 0, 1, $1, 0, 1, $g, 0, 1, $2, 0, 0,
                               0, 1, $h, 0, 1, $5, 0, 0>>}],
                  #decoder{rest = <<1, 2, 3>>}},
                 decode_bin_box(Decoder, Input3)).

decode_13_test() ->
    Decoder = new_decoder(),
    Input = <<0, 0>>,
    ?assertMatch({{[], Decoder}, <<>>},
                 test_one_by_one(Decoder, Input)).

decode_14_test() ->
    Decoder = new_decoder(),
    Input = <<0, 0, 0, 5, "nimbo">>,
    ?assertMatch({{[], Decoder}, <<0, 5, "nimbo">>},
                 test_one_by_one(Decoder, Input)).

decode_15_test() ->
    Decoder = new_decoder(),
    Input = <<0, 7, "billy o", 0, 5, "12345",
              0, 4, "name", 0, 5, "nimbo",
              0, 0>>,
    ?assertMatch({{[{<<"billy o">>, <<"12345">>},
                    {<<"name">>, <<"nimbo">>}], Decoder}, <<>>},
                 test_one_by_one(Decoder, Input)).

-endif.
