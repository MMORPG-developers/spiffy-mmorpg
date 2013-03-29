-module(hello).
{description, "Greet the user"}.

-export([hello/0]).


% This is totally the functional way of doing things.
hello() ->
    io:format("Hello, world!~n").


terminate(_Reason, _State, _Data) ->
    init:stop().

