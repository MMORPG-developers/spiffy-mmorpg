% Record for storing information about a single cell of a map.
-record(map_cell, {
    % blocks_passage: false if actors may freely walk through this square; true
    % otherwise.
    % FIXME: This should be determined by whether there is a wall on the cell.
    blocks_passage = false,
    
    % actors: a list of actors in the cell. The beginning of the list is the
    % "top" of the pile of actors.
    actors = []
}).

% TODO: Have a record for the information that a particular actor knows about
% a map cell?

