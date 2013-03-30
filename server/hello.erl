-module(hello).

-export([hello/0]).


% This is totally the functional way of doing things.
% TODO: Make an executable that calls this.
hello() ->
    io:format("Hello, world!~n").


% This might have something to do with becoming an executable?
terminate(_Reason, _State, _Data) ->
    init:stop().

