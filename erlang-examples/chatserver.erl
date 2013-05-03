-module(chatserver).

% TODO: Look into gen_tcp:shutdown.

% We need to export receivemessages so that spawn can find it.
-export([main/0, receivemessages/2, sendmessages/0]).

-define(PORT, 6667).


% Main (driver) thread

main() -> waitforconnections().

waitforconnections() ->
    Sender = spawn(?MODULE, sendmessages, []),
    {ok, LSock} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
    waitforconnections_helper(LSock, Sender).

waitforconnections_helper(LSock, Sender) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = spawn(?MODULE, receivemessages, [Sock, Sender]),
    Sender ! {connection, {Pid, Sock}},
    waitforconnections_helper(LSock, Sender).


% Receiver threads

receivemessages(Sock, Sender) ->
    ok = receivemessages_helper(Sock, Sender),
    ok = gen_tcp:close(Sock),
    Sender ! {disconnection, {self(), Sock}}.

receivemessages_helper(Sock, Sender) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            Sender ! {newmessage, {self(), Sock}, Data},
            receivemessages_helper(Sock, Sender)
    ;
        {error, closed} ->
            ok
    end.


% Sender thread

sendmessages() -> sendmessages_helper([]).

sendmessages_helper(Connections) ->
    receive
        {connection, NewConnection} ->
            sendmessages_helper([NewConnection|Connections])
    ;
        {disconnection, Disconnected} ->
            sendmessages_helper(Connections -- [Disconnected])
    ;
        {newmessage, _SendingConnection, Data} ->
            ok = broadcast(Connections, Data),
            sendmessages_helper(Connections)
    % TODO: Infinite loop if none of these happens? When do we quit?
    end.

broadcast([], _Data) -> ok;
broadcast([{_Pid, Sock} | Connections], Data) ->
    ok = gen_tcp:send(Sock, Data),
    broadcast(Connections, Data).

