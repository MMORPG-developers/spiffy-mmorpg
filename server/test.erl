-module(test).

% We need to export handle_connection so that spawn can find it.
-export([wait_for_connections/0, handle_connection/1]).

-define(PORT, 6667).


wait_for_connections() ->
    {ok, ListeningSocket} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
    wait_for_connections_helper(ListeningSocket).

wait_for_connections_helper(ListeningSocket) ->
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    spawn(?MODULE, handle_connection, [Socket]),
    wait_for_connections_helper(ListeningSocket).

handle_connection(Socket) ->
    handle_connection_helper(Socket),
    ok = gen_tcp:close(Socket),
    ok.

handle_connection_helper(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            % Don't need a newline (~n) here because Data already has it.
            ok = io:format("Received: ~s", [Data]),
            ok = gen_tcp:send(Socket, Data),
            handle_connection_helper(Socket)
    ;
        {error, closed} ->
            ok
    end.

