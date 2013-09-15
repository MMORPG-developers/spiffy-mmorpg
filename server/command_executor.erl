-module(command_executor).

-export([
    handler/4
]).

-include("map_cell.hrl").



% This function should be used as the handler of a process
% (see inter_process:main_loop/2).
% The resulting process is responsible for executing actions (and checking
% whether they're valid).
% 
% The initial arguments to this process should be two values:
% MapManager, the PID of the map-managing process, and
% TagAssignments, the PID of the tag assignment managing process.
% 
% For subsequent iterations, the Data tuple will contain those same two values.

% Just spawned.
handler({}, setup, _, {MapManager, TagAssignments}) ->
    {handler_continue, {MapManager, TagAssignments}};

% Causes the specified actor to try to walk in the specified direction.
% A walk should always be between two adjacent map cells.
% Tag should be the tag of the actor walking; Direction should be an
% atom indicating the direction to walk (see get_movement_delta/1).
% 
% Currently, no message is sent back to indicate whether the walk
% succeeded.
% FIXME: Is this what we want? It gets somewhat harder to define what
% constitutes "success" for more complex actions, so it's probably best
% to just make the caller figure it out from whether they hear that
% they moved or not. But sending such a message back might make the AI
% much easier.
% 
% FIXME: As there get to be more actions, divide the handler code among many
% helper functions, and delegate some of the pattern-matching to them as well.
handler(Data = {MapManager, TagAssignments}, notification, action,
        {Tag, walk, {Direction}}) ->
    % Translate the Tag and Direction.
    {ok, ActorInfo} = inter_process:make_request(
        TagAssignments, lookup_tag, {Tag}),
    {DeltaRows, DeltaColumns} = get_movement_delta(Direction),
    
    % Calculate the destination location.
    {ok, {OldRow, OldColumn}} = inter_process:make_request(
        ActorInfo, get_position, {}),
    NewRow = OldRow + DeltaRows,
    NewColumn = OldColumn + DeltaColumns,
    NewPosition = {NewRow, NewColumn},
    
    % Check if the Actor can walk into the destination cell or not.
    % Note that since walking is by definition between adjacent cells,
    % we need not check if there are cells in between.
    {ok, DestinationCell} = inter_process:make_request(
        MapManager, get_cell, {NewPosition}),
    case DestinationCell#map_cell.blocks_passage of
        false ->
            % The actor can move into that square.
            inter_process:notify(
                MapManager, move_actor, {ActorInfo, NewPosition})
    ;
        true ->
            % The actor cannot move into that square.
            % In this case, simply do nothing.
            ok
    end,
    
    {handler_continue, Data}.



% Private/helper Functions



% get_movement_delta(Direction)
% Returns the displacement induced by moving once in the specified Direction.
% Direction should be an atom, one of:
%     north, east, south, west, northeast, southeast, southwest, northwest.
% Return format is {DeltaRows, DeltaColumns}, where DeltaRows is the change in
% the walker's row and DeltaColumns is the change in the walker's column.
get_movement_delta(north) ->
    {-1,  0};
get_movement_delta(northeast) ->
    {-1,  1};
get_movement_delta(east) ->
    { 0,  1};
get_movement_delta(southeast) ->
    { 1,  1};
get_movement_delta(south) ->
    { 1,  0};
get_movement_delta(southwest) ->
    { 1, -1};
get_movement_delta(west) ->
    { 0, -1};
get_movement_delta(northwest) ->
    {-1, -1}.

