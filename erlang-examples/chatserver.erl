-module(chatserver).

% TODO: Look into gen_tcp:shutdown.

% We need to export handleconnection so that spawn can find it.
-export([waitforconnections/0, handleconnection/1]).

% -import(string)

-define(PORT, 6667).


waitforconnections() ->
    {ok, LSock} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
    waitforconnections_helper(LSock).

waitforconnections_helper(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(?MODULE, handleconnection, [Sock]),
    waitforconnections_helper(LSock).

handleconnection(Sock) ->
    handleconnection_helper(Sock),
    ok = gen_tcp:close(Sock),
    ok.

handleconnection_helper(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            % Don't need a newline (~n) here because Data already has it.
            ok = io:format("~s~s", ["Received: ", Data]),
            ok = gen_tcp:send(Sock, Data),
            handleconnection_helper(Sock)
    ;
        {error, closed} ->
            ok
    end.

