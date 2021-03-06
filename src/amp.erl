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
-export([listen/3]).
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

-type endpoint() :: {tcp | ssl,
                     inet:ip_address() | inet:hostname(),
                     inet:port_number(),
                     Options::any()}.


-spec connect(endpoint(), any()) -> ok.
connect({ConnectionType, Host, Port, ConnectOpts}, Opts) ->
    Ref = make_ref(),
    case ranch_tcp:connect(Host, Port, ConnectOpts) of
        {ok, Socket} ->
            start_conn_server(conn_transport(ConnectionType), Socket, amp_server,
                              [{connect_client, self(), Ref} | Opts]);
        Error ->
            Error
    end.

listen(ConnectionType, TransportOpts, AmpOpts) ->
    Name = make_ref(),
    {ok, _} = ranch:start_listener(Name, 100, listen_transport(ConnectionType),
                                   TransportOpts, amp_server, AmpOpts),
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

conn_transport(tcp) ->
    amp_null_tcp;
conn_transport(ssl) ->
    amp_null_ssl.

listen_transport(tcp) ->
    ranch_tcp;
listen_transport(ssl) ->
    ranch_ssl.
