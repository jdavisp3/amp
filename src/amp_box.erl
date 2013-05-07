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
         new_decoder/1, decode_box/2,
         decode_header/1, decode_command_header/1,
         encode_box/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(decoder, {
          orig_protocol,
          protocol,
          remainder,
          box=[]
         }).

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


%% @doc Given an amp_command record, a message id, and a box, return a
%% binary encoding of the Amp Box that would implement the call.
-spec encode_ask(amp_command:amp_command(), binary(), amp:amp_box()) -> binary().
encode_ask(Command, Id, Box) ->
    [_ | _] = Box, % no empty boxes
    encode_box([{?AMP_KEY_ASK, string, []},
                {?AMP_KEY_COMMAND, string, []}
                | amp_command:arguments(Command)],
               [{?AMP_KEY_ASK, Id},
                {?AMP_KEY_COMMAND, amp_command:name(Command)}
                | Box]).


%% @doc Given an amp_command record, a message id, and a box, return a
%% binary encoding of the AmpBox that would implement the answer box for
%% a call.
-spec encode_answer(amp_command:amp_command(), binary(),
                    amp:amp_box()) -> binary().
encode_answer(Command, Id, Box) ->
    [_ | _] = Box, % no empty boxes
    encode_box([{?AMP_KEY_ANSWER, string, []} | amp_command:response(Command)],
               [{?AMP_KEY_ANSWER, Id} | Box]).

%% @doc Given an amp_command record, a message id, and an error code,
%% and a description, return a binary encoding of the Amp Box that
%% would implement the error box for a call.
-spec encode_error(amp_command:amp_command(), binary(),
                   binary(), binary()) -> binary().
encode_error(Command, Id, ErrorCode, Description) ->
    {value, _} = lists:keysearch(ErrorCode, 1, amp_command:errors(Command)),
    encode_box([{?AMP_KEY_ERROR, string, []} | ?AMP_ERROR_PROTOCOL],
               [{?AMP_KEY_ERROR, Id},
                {?AMP_KEY_ERROR_CODE, ErrorCode},
                {?AMP_KEY_ERROR_DESCRIPTION, Description}]).


%% @doc Return a new decoder object suitable for unserializing a wire
%% format of an Amp box. Decoders are required arguments for decode_box/2.
-spec new_decoder(amp_command:amp_list()) -> #decoder{}.
new_decoder(Protocol) ->
    [_ | _] = Protocol, % no empty boxes
    #decoder{orig_protocol=Protocol, protocol=Protocol, remainder= <<>>}.

%% @doc Decode a part (or whole) of a box.
%% If the complete box is decoded, return the box we decoded and the
%% unprocessed bytes. If we are not done, return the new state of the
%% decoder.
-spec decode_box(#decoder{}, binary()) ->
                        {not_done, #decoder{}} |
                        {done, amp:amp_box(), Rest::binary()}.
decode_box(Decoder, Packet) ->
    Remainder = Decoder#decoder.remainder,
    Whole = <<Remainder/binary, Packet/binary>>,
    Result = decode_box(Decoder#decoder.protocol,
                        Decoder#decoder.box, Whole),
    case Result of
        {not_done, Protocol, Box, Rest} ->
            {not_done, Decoder#decoder{protocol=Protocol, box=Box,
                                       remainder=Rest}};
        {done, Box, Rest} ->
            [_ | _] = Box, % no empty boxes
            {done, lists:reverse(Box), Rest}
    end.

%% @doc Match a key/value pair encoded at the front of an incoming box.
%% Return a tuple indicating the type of incoming box and the Id for the
%% message, plus the remaining bytes in the packet. The function will crash
%% if the kvp has the wrong name.
-spec decode_header(binary())
                   -> 'not_enough'
                          | {'ask' | 'answer' | 'error',
                             Id::binary(),
                             Remaining::binary()}.
decode_header(Packet) ->
    case match_kvp(Packet) of
        not_enough ->
            not_enough;
        {Key, ValBin, Remaining} ->
            Id = decode_value(ValBin, binary),
            BoxType = box_type(Key),
            {BoxType, Id, Remaining}
    end.

%% @doc Match a key/value pair encoding the command name of an ask box.
%% Return a tuple with the command name and the remaining bytes in the
%% packet. The function will crash if the kvp was the wrong name.
-spec decode_command_header(binary())
                           -> not_enough
                                  | {CommandName::binary(), Remaining::binary()}.
decode_command_header(Packet) ->
    case match_kvp(Packet) of
        not_enough ->
            not_enough;
        {?AMP_KEY_COMMAND, ValBin, Remaining} ->
            CommandName = decode_value(ValBin, binary),
            {CommandName, Remaining}
    end.

%% @doc Given an AmpList protocol and a box, return a binary encoding
%% of the box that matches the protocol.
-spec encode_box(amp_command:amp_list(), amp:amp_box()) -> binary().
encode_box(Protocol, Box) ->
    IOList = encode_box_int(Protocol, Box),
    [_, _ | _] = IOList, % no empty boxes
    list_to_binary(IOList).


% @private
% @doc Encode the box according to the given protocol into the IOList.
-spec encode_box_int(amp_command:amp_list(), amp:amp_box()) -> iolist().
encode_box_int([], _Box) ->
    [<<0, 0>>];
encode_box_int([{Key, Type, Options} | Protocol], Box) ->
    case lists:keysearch(Key, 1, Box) of
        {value, {Key, Value}} ->
            <<_, _/binary>> = Key, % no empty keys
            EncKeyLength = encode_length(size(Key), ?AMP_MAX_KEY_LEN),
            EncValue = encode_value(Value, Type),
            EncLength = encode_length(iolist_size(EncValue), ?AMP_MAX_VAL_LEN),
            [EncKeyLength, Key, EncLength, EncValue
             | encode_box_int(Protocol, Box)];
        false ->
            true = proplists:get_bool(optional, Options),
            encode_box_int(Protocol, Box)
    end.

% @private
% @doc Decode the packet as much as possible and return the results.
-spec decode_box(amp_command:amp_list(), amp:amp_box(), binary()) ->
                        {not_done, amp_command:amp_list(),
                         amp:amp_box(), Rest::binary()}
                            | {done, amp:amp_box(), Rest::binary()}.
decode_box(Protocol, Box, Packet) when size(Packet) < 2 ->
    {not_done, Protocol, Box, Packet};
decode_box([], Box, <<0, 0, Rest/binary>>) ->
    {done, Box, Rest};
decode_box([{_, _, Options} | Protocol], Box, <<0, 0, _/binary>> = Packet) ->
    true = proplists:get_bool(optional, Options),
    decode_box(Protocol, Box, Packet);
decode_box(Protocol, Box, Packet) ->
    case match_kvp(Packet) of
        not_enough ->
            {not_done, Protocol, Box, Packet};
        {Key, ValBin, Rest} ->
            {Type, NewProtocol} = consume_key(Key, Protocol),
            Value = decode_value(ValBin, Type),
            decode_box(NewProtocol, [{Key, Value} | Box], Rest)
    end.

% @private
% @doc Match a key/value pair encoded at the front of Packet.
% Return a tuple with the key name, the value still encoded as a binary,
% and the remaining bytes from Packet that were not used in the kvp. Or,
% if there are not enough bytes to decode the first kvp, return not_enough.
-spec match_kvp(binary()) -> {Key::amp_command:amp_name(),
                              ValBin::binary(),
                              Rest::binary()} | 'not_enough'.
match_kvp(<<0, KeyLen, Key:KeyLen/binary,
            ValLen:16/unsigned-big, Val:ValLen/binary, Rest/binary>>) ->
    {Key, Val, Rest};
match_kvp(<<0, _/binary>>) ->
    not_enough;
match_kvp(<<>>) ->
    not_enough.


% @private
% @doc Consume the given key from the Protocol and return the
% type of that key and the remaining protocol.
-spec consume_key(binary(), amp_command:amp_list()) ->
                         {amp_command:amp_type(),
                          amp_command:amp_list()}.
consume_key(Key, Protocol) ->
    {value, {Key, Type, _Options}, Protocol2} = lists:keytake(Key, 1, Protocol),
    {Type, Protocol2}.


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
decode_amplist(ValBin, Protocol) ->
    {done, KVPairs, Rest} = decode_box(Protocol, [], ValBin),
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
% @doc Return a box type atom given the key.
-spec box_type(binary()) -> atom().
box_type(?AMP_KEY_ASK) ->
    ask;
box_type(?AMP_KEY_ANSWER) ->
    answer;
box_type(?AMP_KEY_ERROR) ->
    error.


% Tests

-ifdef(TEST).

encode_test_() ->
    [
     ?_assertMatch(<<0, 1, $a, 0, 1, $1, 0, 0>>,
                   encode_box([{<<"a">>, integer, []}], [{<<"a">>, 1}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{<<"aa">>, string, []}], [{<<"aa">>, "xyz"}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{<<"aa">>, string, []}], [{<<"aa">>, <<"xyz">>}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{<<"aa">>, string, []}], [{<<"aa">>, "xyz"}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{<<"aa">>, binary, []}], [{<<"aa">>, <<"xyz">>}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{<<"aa">>, binary, []}], [{<<"aa">>, "xyz"}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{<<"aa">>, binary, []}], [{<<"aa">>, <<"xyz">>}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 26, "1.50000000000000000000e+00", 0, 0>>,
                   encode_box([{<<"aa">>, float, []}], [{<<"aa">>, 1.5}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 26, "1.00000000000000000000e+00", 0, 0>>,
                   encode_box([{<<"aa">>, float, []}], [{<<"aa">>, 1}])),
     ?_assertMatch(<<0, 1, $a, 0, 4, "True", 0, 0>>,
                   encode_box([{<<"a">>, boolean, []}], [{<<"a">>, true}])),
     ?_assertMatch(<<0, 1, $a, 0, 5, "False", 0, 0>>,
                   encode_box([{<<"a">>, boolean, []}], [{<<"a">>, false}])),
     ?_assertMatch(<<0, 1, $a, 0, 1, $1, 0, 0>>,
                   encode_box([{<<"b">>, integer, [optional]},
                               {<<"a">>, integer, []}],
                               [{<<"a">>, 1}])),
     ?_assertMatch(<<0, 1, $b, 0, 1, $2, 0, 1, $a, 0, 1, $1, 0, 0>>,
                   encode_box([{<<"b">>, integer, [optional]}, {<<"a">>, integer, []}],
                               [{<<"a">>, 1}, {<<"b">>, 2}])),
     ?_assertMatch(<<0, 1, $a, 0, 8, 0, 1, $b, 0, 1, $5, 0, 0, 0, 0>>,
                   encode_box([{<<"a">>, {amplist, [{<<"b">>, integer, []}]}, []}],
                              [{<<"a">>, [[{<<"b">>, 5}]]}])),
     ?_assertMatch(<<0, 1, $a, 0, 16, 0, 1, $b, 0, 1, $5, 0, 0,
                                      0, 1, $b, 0, 1, $0, 0, 0, 0, 0>>,
                   encode_box([{<<"a">>, {amplist, [{<<"b">>, integer, []}]}, []}],
                              [{<<"a">>, [[{<<"b">>, 5}], [{<<"b">>, 0}]]}])),
     ?_assertError(_, encode_box([{[256], string, []}], [{[256], "xyz"}])),
     ?_assertError(_, encode_box([{<<"aa">>, string, []}], [{<<"aa">>, 1}])),
     ?_assertError(_, encode_box([{<<"">>, string, []}], [{<<"">>, "a"}])),
     ?_assertError(_, encode_box([{<<"aa">>, float, []}], [{<<"aa">>, "apple"}])),
     ?_assertError(_, encode_box([{<<"b">>, integer, [optional]}], [{<<"a">>, 1}])),
     ?_assertError(_, encode_box([{<<"b">>, integer, [optional]}], []))
    ].

encode_ask_test() ->
    Cmd = amp_command:new(<<"n">>, [{<<"a">>, string, []}], nil, nil, []),
    Bin = encode_ask(Cmd, <<"1">>, [{<<"a">>, "A"}]),
    ?assertMatch(Bin, <<0, 4, "_ask", 0, 1, "1",
                        0, 8, "_command", 0, 1, "n",
                        0, 1, "a", 0, 1, "A", 0, 0>>),
    ?assertMatch({ask, <<"1">>,
                  <<0, 8, "_command", 0, 1, "n",
                    0, 1, "a", 0, 1, "A", 0, 0>>},
                 decode_header(Bin)),
    ?assertMatch({<<"n">>, <<0, 1, "a", 0, 1, "A", 0, 0>>},
                 decode_command_header(<<0, 8, "_command", 0, 1, "n",
                                         0, 1, "a", 0, 1, "A", 0, 0>>)),
    ?assertMatch(not_enough, 
                 decode_command_header(<<0, 8, "_command", 0, 1>>)).

encode_answer_test() ->
    Cmd = amp_command:new(<<"n">>, nil,
                          [{<<"b">>, string, []}], [], []),
    Bin = encode_answer(Cmd, <<"1">>, [{<<"b">>, "B"}]),
    ?assertMatch(Bin, <<0, 7, "_answer", 0, 1, "1",
                        0, 1, "b", 0, 1, "B", 0, 0>>),
    ?assertMatch({answer, <<"1">>, _}, decode_header(Bin)).

encode_error_test() ->
    Cmd = amp_command:new(<<"n">>, nil, nil, 
                          [{<<"A">>, []},
                           {<<"B">>, [fatal]}], []),
    Bin1 = encode_error(Cmd, "1", <<"A">>, <<"AA">>),
    ?assertMatch(Bin1, <<0, 6, "_error", 0, 1, "1",
                         0, 11, "_error_code", 0, 1, "A",
                         0, 18, "_error_description", 0, 2, "AA", 0, 0>>),
    {error, <<"1">>, Rest1} = decode_header(Bin1),
    Decoder = new_decoder(?AMP_ERROR_PROTOCOL),
    Out1 = {done, [{?AMP_KEY_ERROR_CODE, "A"},
                   {?AMP_KEY_ERROR_DESCRIPTION, "AA"}], <<>>},
    ?assertMatch(Out1, decode_box(Decoder, Rest1)),

    Bin2 = encode_error(Cmd, "2", <<"B">>, <<"BB">>),
    {error, <<"2">>, Rest2} = decode_header(Bin2),
    Out2 = {done, [{?AMP_KEY_ERROR_CODE, "B"},
                   {?AMP_KEY_ERROR_DESCRIPTION, "BB"}], <<>>},
    ?assertMatch(Out2, decode_box(Decoder, Rest2)).


test_one_by_one(Decoder, <<Byte, Input/binary>>) ->
    case decode_box(Decoder, <<Byte>>) of
        {not_done, Decoder1} ->
            test_one_by_one(Decoder1, Input);
        Result ->
            Result
    end.

decode_1_test() ->
    Protocol = [{"a", integer, []}],
    Decoder1 = new_decoder(Protocol),
    {not_done, Decoder2} = decode_box(Decoder1, <<0>>),
    ?assert(Decoder2#decoder.protocol == Protocol),
    ?assert(Decoder2#decoder.remainder == <<0>>),
    ?assert(Decoder2#decoder.box == []),

    {not_done, Decoder3} = decode_box(Decoder2, <<1, $a>>),

    ?assert(Decoder2#decoder.protocol == Protocol),
    ?assert(Decoder3#decoder.remainder == <<0, 1, $a>>),
    ?assert(Decoder3#decoder.box == []).

decode_2_test() ->
    Protocol = [{<<"a">>, integer, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 1, $a, 0, 1, $1, 0, 0>>,
    ?assertMatch({done, [{<<"a">>, 1}], <<>>},
                 decode_box(Decoder, Input)).

decode_3_test() ->
    Protocol = [{<<"a">>, integer, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 1, $a, 0, 1, $4, 0, 0, 14>>,
    ?assertMatch({done, [{<<"a">>, 4}], <<14>>},
                 decode_box(Decoder, Input)).

decode_4_test() ->
    Protocol = [{<<"name">>, string, []},
                {<<"billy o">>, integer, []}],
    Decoder = new_decoder(Protocol),
    Input1 = <<0, 7, "billy o", 0, 5, "12345",
               0, 4, "name", 0, 5, "nimbo",
               0, 0>>,
    Output = {done, [{<<"billy o">>, 12345}, {<<"name">>, "nimbo"}], <<>>},
    ?assertMatch(Output, test_one_by_one(Decoder, Input1)),
    Input2 = <<0, 4, "name", 0, 5, "nimbo",
               0, 7, "billy o", 0, 5, "12345",
               0, 0>>,
    Output2 = {done, [{<<"name">>, "nimbo"}, {<<"billy o">>, 12345}], <<>>},
    ?assertMatch(Output2, test_one_by_one(Decoder, Input2)).

decode_5_test() ->
    Protocol = [{<<"name">>, string, []}],
    Decoder = new_decoder(Protocol),
    Input = <<1, 4, "name", 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_6_test() ->
    Protocol = [{<<"name">>, string, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 4, "namx", 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_7_test() ->
    Protocol = [{<<"q">>, integer, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 1, "q", 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_8_test() ->
    Protocol = [{<<";">>, float, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 1, $;, 0, 3, "1.5", 0, 0, 1, 2, 3>>,
    ?assertMatch({done, [{<<";">>, 1.5}], <<1, 2, 3>>},
                 decode_box(Decoder, Input)),
    Input2 = <<0, 1, $;, 0, 1, "1", 0, 0, 1, 2, 3>>,
    ?assertMatch({done, [{<<";">>, 1.0}], <<1, 2, 3>>},
                 decode_box(Decoder, Input2)).

decode_9_test() ->
    Protocol = [{<<"//">>, boolean, []}],
    Decoder = new_decoder(Protocol),
    Input1 = <<0, 2, "//", 0, 4, "True", 0, 0>>,
    ?assertMatch({done, [{<<"//">>, true}], <<>>},
                 decode_box(Decoder, Input1)),
    Input2 = <<0, 2, "//", 0, 5, "False", 0, 0>>,
    ?assertMatch({done, [{<<"//">>, false}], <<>>},
                 decode_box(Decoder, Input2)).

decode_10_test() ->
    Protocol = [{<<"a">>, integer, [optional]},
                {<<"b">>, integer, []}],
    Decoder = new_decoder(Protocol),
    Input1 = <<0, 1, $a, 0, 1, $4, 0, 1, $b, 0, 1, $5, 0, 0>>,
    ?assertMatch({done, [{<<"a">>, 4}, {<<"b">>, 5}], <<>>},
                 decode_box(Decoder, Input1)),
    Input2 = <<0, 1, $b, 0, 1, $5, 0, 0>>,
    ?assertMatch({done, [{<<"b">>, 5}], <<>>},
                 decode_box(Decoder, Input2)).

decode_11_test() ->
    Protocol = [{<<"a">>, integer, []},
                {<<"b">>, integer, [optional]}],
    Decoder = new_decoder(Protocol),
    Input1 = <<0, 1, $a, 0, 1, $4, 0, 1, $b, 0, 1, $5, 0, 0>>,
    ?assertMatch({done, [{<<"a">>, 4}, {<<"b">>, 5}], <<>>},
                 decode_box(Decoder, Input1)),
    Input2 = <<0, 1, $a, 0, 1, $5, 0, 0>>,
    ?assertMatch({done, [{<<"a">>, 5}], <<>>},
                 decode_box(Decoder, Input2)).

decode_12_test() ->
    SubProto = [{<<"h">>, integer, []},
                {<<"g">>, integer, [optional]}],
    Protocol = [{<<"'">>, integer, []},
                {<<"s">>, {amplist, SubProto}, [optional]}],
    Decoder = new_decoder(Protocol),

    Input1 = <<0, 1, $', 0, 1, $0, 0, 0, 1, 2, 3>>,
    ?assertMatch({done, [{<<"'">>, 0}], <<1, 2, 3>>},
                 decode_box(Decoder, Input1)),

    Input2 = <<0, 1, $', 0, 1, $0,
              0, 1, $s, 0, 14,
                0, 1, $h, 0, 1, $1, 0, 1, $g, 0, 1, $2, 0, 0,
              0, 0, 1, 2, 3>>,
    Output2 = [{<<"'">>, 0}, {<<"s">>, [[{<<"h">>, 1}, {<<"g">>, 2}]]}],
    ?assertMatch({done, Output2, <<1, 2, 3>>}, decode_box(Decoder, Input2)),

    Input3 = <<0, 1, $', 0, 1, $0,
              0, 1, $s, 0, 22,
                0, 1, $h, 0, 1, $1, 0, 1, $g, 0, 1, $2, 0, 0,
                0, 1, $h, 0, 1, $5, 0, 0,
              0, 0, 1, 2, 3>>,
    Output3 = [{<<"'">>, 0}, {<<"s">>, [[{<<"h">>, 1},
                                         {<<"g">>, 2}], [{<<"h">>, 5}]]}],
    ?assertMatch({done, Output3, <<1, 2, 3>>}, decode_box(Decoder, Input3)).

decode_13_test() ->
    Protocol = [{"name", string, [optional]}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 0>>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_14_test() ->
    Protocol = [{"", string, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 0, 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_15_test() ->
    Protocol = [{<<"name">>, binary, []},
                {<<"billy o">>, integer, []}],
    Decoder = new_decoder(Protocol),
    Input1 = <<0, 7, "billy o", 0, 5, "12345",
               0, 4, "name", 0, 5, "nimbo",
               0, 0>>,
    Output = {done, [{<<"billy o">>, 12345}, {<<"name">>, <<"nimbo">>}], <<>>},
    ?assertMatch(Output, test_one_by_one(Decoder, Input1)).

decode_header_test_() ->
    [
     ?_assertMatch(not_enough, decode_header(<<>>)),
     ?_assertMatch(not_enough, decode_header(<<0, 4, "_ask">>)),
     ?_assertMatch({ask, <<"a">>, <<>>},
                   decode_header(<<0, 4, "_ask", 0, 1, $a>>)),
     ?_assertMatch({ask, <<"a">>, <<9>>},
                   decode_header(<<0, 4, "_ask", 0, 1, $a, 9>>)),
     ?_assertMatch({answer, <<"a">>, <<>>},
                   decode_header(<<0, 7, "_answer", 0, 1, $a>>)),
     ?_assertMatch({error, <<"a">>, <<>>},
                   decode_header(<<0, 6, "_error", 0, 1, $a>>)),
     ?_assertError(_, decode_header(<<0, 4, "_bad", 0, 1, $a>>))
     ].

-endif.
