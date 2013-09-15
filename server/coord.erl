-module(coord).

-export([
    subtract/2
]).



% Return Minuend - Subtrahend.
% Minuend and Subtrahend should be coordinates, given as 2-tuples.
subtract(Minuend, Subtrahend) ->
    % Unpack the inputs.
    {MRow, MColumn} = Minuend,
    {SRow, SColumn} = Subtrahend,
    
    % Subtract each coordinate.
    DeltaRows = MRow - SRow,
    DeltaColumns = MColumn - SColumn,
    
    % Return the difference.
    {DeltaRows, DeltaColumns}.

% FIXME: Consider using records instead of tuples.

% TODO: Provide an addition function. Also maybe other functions.

