-module(factorial).
{description, "Calculate factorials"}

-export([fact/1]).


% Silly recursive implementation of factorial. Clearly the better solution
% would be to make it a 2-argument tail-recursive function. Because tail
% recursion is the solution to all problems in functional programs.

fact(N) when N > 0 ->
    N * fact(N - 1);
fact(0) ->
    1.


terminate(_Reason, _State, _Data) ->
    init:stop().

