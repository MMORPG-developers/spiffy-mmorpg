% This is largely copied from
%   http://www.erlang.org/doc/tutorial/c_port.html

-module(caller).

-export([
% For external use
    start/1,
    stop/0,
    foo/1,
    bar/1,
% For spawning
    init/1
]).


% Setting up the port.

start(Program) ->
    spawn(?MODULE, init, [Program]).

stop() ->
    library ! stop.

init(Program) ->
    % So that we can hereafter use `library ! <message>` instead of trying to
    % pass around the PID.
    register(library, self()),
    
    % Something to do with catching the exit signal.
    process_flag(trap_exit, true),
    
    % Actually open the port.
    % The option {packet, 2} means automatically include a 2-byte header that
    % encodes the size of the message when communicating over the port.
    Port = open_port({spawn, Program}, [{packet, 2}]),
    
    % Enter main loop.
    loop(Port).


% Wrappers around the C functions.

foo(X) ->
    call_port({foo, X}).

bar(Y) ->
    call_port({bar, Y}).


% Encoding and decoding data so the C program can read it.

encode({foo, X}) -> [1, X];
encode({bar, Y}) -> [2, Y].

%%% These also work; you can use binaries instead of lists when encoding.
% encode({foo, X}) -> <<1:8, X:8>>;
% encode({bar, Y}) -> <<2:8, Y:8>>.

% However, decode/1 must take a list as an argument, not a binary. Apparently
% the port sends its response as a list of bytes and not a binary.

decode([Int]) -> Int.


% More encoding or something like that I guess.

call_port(Msg) ->
    library ! {call, self(), Msg},
    receive
        {return, Result} ->
            Result
    end.


% Main loop.

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {return, decode(Data)}
            end,
            loop(Port)
    ;
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end
    ;
        {'EXIT', Port, _Reason} ->
            exit(port_terminated)
    end.

