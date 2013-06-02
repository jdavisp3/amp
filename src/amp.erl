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

-module(amp).

-export([connect/2]).
-export([listen/2]).
-export([stop_listening/1]).

-export_type([amp_box/0, amp_bin_box/0]).

-export_type([amp_type/0, amp_list/0, amp_name/0,
              amp_option/0, amp_error/0, amp_error_option/0,
              amp_command_option/0, amp_command/0]).

-type amp_box() :: [{binary(), any()}].
-type amp_bin_box() :: [{binary(), binary()}].

-type amp_type() :: 'string' | 'binary' | 'integer' | 'float' | 'boolean'
                  | {'amplist', amp_list()}.
-type amp_list() :: [{amp_name(), amp_type(), [amp_option()]}].
-type amp_name() :: binary().
-type amp_option() :: 'optional'.

-type amp_error() :: {amp_name(), [amp_error_option()]}.
-type amp_error_option() :: 'fatal'.

-type amp_command_option() :: 'requires_answer'.

-opaque amp_command() :: record().

-type endpoint() :: {tcp, inet:ip_address() | inet:hostname(),
                     inet:port_number(), Options::any()}
                  | {ssl, inet:ip_address() | inet:hostname(),
                     inet:port_number(), Options::any()}.


-spec connect(endpoint(), any()) -> {ok, pid()} | {error, atom()}.
connect({tcp, Host, Port, ConnectOpts}, Opts) ->
    case ranch_tcp:connect(Host, Port, ConnectOpts) of
        {ok, Socket} ->
            start_conn_server(ranch_tcp, Socket, amp_server, Opts);
        Error ->
            Error
    end.

listen(ConnectionType, Opts) ->
    Name = make_ref(),
    RanchOpts = proplists:get_value(ranch_opts, Opts, []),
    {ok, _} = ranch:start_listener(Name, 100, ranch_handler(ConnectionType),
                                   RanchOpts, amp_server, Opts),
    {ok, ranch:get_port(Name), Name}.

stop_listening(Name) ->
    ranch:stop_listener(Name).


start_conn_server(Transport, Socket, Protocol, ProtoOpts) ->
    Ref = {ampconn, Transport},
    case ranch:start_listener(Ref, 0, Transport, [], Protocol, ProtoOpts) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok
    end,
    SupPid = ranch_server:get_connections_sup(Ref),
    ranch_conns_sup:start_protocol(SupPid, Socket).

ranch_handler(tcp) ->
    ranch_tcp;
ranch_handler(ssl) ->
    ranch_ssl.
