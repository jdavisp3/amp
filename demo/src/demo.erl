%% Reuse at will.

%% @private
-module(demo).

%% API
-export([sum/4]).
-export([divide/4]).
-export([start/0]).


-spec sum(inet:ip_address() | inet:hostname(), inet:port_number(),
          integer(), integer()) -> integer().
sum(Address, Port, N1, N2) ->
    ok.

-spec divide(inet:ip_address() | inet:hostname(), inet:port_number(),
             integer(), integer()) -> integer().
divide(Address, Port, N1, N2) ->
    ok.

start() ->
    ok = application:start(ranch),
    {ok, Port, _} = amp:listen(tcp, [{ranch_opts, [{port, 1234}]},
                                     {handler, demo_handler}]),
    io:format("Listening on ~p.~n", [Port]).
