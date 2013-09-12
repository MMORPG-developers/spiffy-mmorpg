-module(map_manager).

-export([
    handler/4
]).

-include("map_cell.hrl").


% This function should be used as the handler of a process
% (see inter_process:main_loop/2).
% The resulting process serves as the authority on what's at each cell of the
% map.
% 
% The initial arguments to this process should be a single value, Size, which
% should be the dimensions of the map given as {Rows, Columns}.
% 
% For subsequent iterations, the Data tuple will contain two values:
% the actual map array and a list of processes to be notified when the map
% changes.
% 
% Makes blocking requests of various actor info processes.

% Just spawned.
handler({}, setup, _, {Size}) ->
    % Initialize the map.
    Map = create_map(Size),
    
    {handler_continue, {Map, []}};

% Subscribe a new process to notifications about changes in the map.
handler({Map, NotifiedProcesses}, notification, subscribe,
        {NewProcess}) ->
    {handler_continue, {Map, [NewProcess|NotifiedProcesses]}};

% Unsubscribe a process from notifications about changes in the map.
handler({Map, NotifiedProcesses},
        notification, unsubscribe, {Process}) ->
    {handler_continue, {Map, NotifiedProcesses -- [Process]}};

% Someone's requested the size of the map.
handler(Data = {Map, _NotifiedProcesses}, request, get_size,
        {}) ->
    Size = array_2d:size(Map),
    
    % Return it to them, in the form {Rows, Columns}.
    {handler_continue, Data, {ok, Size}};

% Someone's requested a particular map cell.
handler(Data = {Map, _NotifiedProcesses}, request, get_cell,
        {Position}) ->
    Cell = array_2d:get(Position, Map),
    
    % Return it to them, in the form {Rows, Columns}.
    {handler_continue, Data, {ok, Cell}};

% Put a new actor in the map.
% Position is the position at which to place the actor.
% ActorInfo is the PID of the process storing information on the actor.
% FIXME: Use a tag instead of the PID of the actor info process.
handler({Map, NotifiedProcesses}, notification, new_actor,
        {Position, ActorInfo}) ->
    % Grab a copy of the map cell before we remove the actor.
    CellBefore = array_2d:get(Position, Map),
    
    % The real work is done by a helper function.
    % FIXME: The helper function and the message should probably take
    % their arguments in the same order.
    NewMap = add_actor_to_map(Map, ActorInfo, Position),
    
    % Get a copy of the map cell after we've removed the actor.
    CellAfter = array_2d:get(Position, NewMap),
    
    % Inform everyone else the cell's been updated.
    inter_process:notify_all(NotifiedProcesses, update_map_cell,
        {Position, CellBefore, CellAfter}),
    
    {handler_continue, {NewMap, NotifiedProcesses}};

% Remove an actor from the map.
% ActorInfo is the PID of the process storing information on the actor.
% FIXME: Use a tag instead of the PID of the actor info process.
handler({Map, NotifiedProcesses}, notification, remove_actor,
        {ActorInfo}) ->
    % Figure out where the actor is.
    {ok, Position} = inter_process:make_request(ActorInfo, get_position, {}),
    
    % Grab a copy of the map cell before we remove the actor.
    CellBefore = array_2d:get(Position, Map),
    
    % Remove the actor from the cell.
    % FIXME: Consistency in argument order (see new_actor).
    NewMap = remove_actor_from_map(Map, ActorInfo, Position),
    
    % Get a copy of the map cell after we've removed the actor.
    CellAfter = array_2d:get(Position, NewMap),
    
    % Inform everyone else the cell's been updated.
    inter_process:notify_all(NotifiedProcesses, update_map_cell,
        {Position, CellBefore, CellAfter}),
    
    {handler_continue, {NewMap, NotifiedProcesses}};

% Move an actor to a new location.
% ActorInfo is the PID of the process storing information on the actor.
% NewPosition is the position to which to move the actor.
handler({Map, NotifiedProcesses}, notification, move_actor,
        {ActorInfo, NewPosition}) ->
    % Figure out the actor's old position. We trust the ActorInfo
    % process to store the correct position, so just ask it.
    {ok, OldPosition} = inter_process:make_request(
        ActorInfo, get_position, {}),
    
    % Grab copies of the map_cell records for the start and end cells
    % before they're changed to reflect the movement of the actor.
    StartCellBefore = array_2d:get(OldPosition, Map),
    EndCellBefore = array_2d:get(NewPosition, Map),
    
    % Remove the actor from the map, then put it back in at the new
    % location.
    MapWithoutActor = remove_actor_from_map(Map, ActorInfo,
                                            OldPosition),
    MapWithActorMoved = add_actor_to_map(MapWithoutActor, ActorInfo,
                                         NewPosition),
    
    % Update the actor's position.
    % FIXME: Don't obscure the fact that that third argument is a
    % tuple. Actually, scratch that. Make the ActorInfo take a single argument
    % that's a tuple instead of 2 separate arguments.
    inter_process:send_notification(ActorInfo,
        move_in_map, NewPosition),
    
    % Get copies of the map_cell records for the start and end cells
    % after they've been changed to reflect the movement of the actor.
    StartCellAfter = array_2d:get(OldPosition, MapWithActorMoved),
    EndCellAfter = array_2d:get(NewPosition, MapWithActorMoved),
    
    % Notify anyone who should know that the actor moved.
    inter_process:notify_all(NotifiedProcesses, actor_moved,
        {ActorInfo, OldPosition, NewPosition}),
    
    % Notify anyone who should know that the start and end cells have
    % been updated.
    inter_process:notify_all(NotifiedProcesses, update_map_cell,
        {OldPosition, StartCellBefore, StartCellAfter}),
    inter_process:notify_all(NotifiedProcesses, update_map_cell,
        {NewPosition, EndCellBefore, EndCellAfter}),
    
    {handler_continue, {MapWithActorMoved, NotifiedProcesses}}.



% Private/helper functions.



% create_map(Size)
% Creates an empty map of the specified size.
% Size is given in the form {Rows, Columns}.
create_map(Size = {Rows, Columns}) ->
    % Create an empty 2D array of that size.
    UninitializedMap = array_2d:new(Size),
    
    % Use array_2d:map to fill it with floors and walls.
    FillFunction = fun
        ({R, _}, _) when (R == 0) or (R == Rows-1) ->
            #map_cell{blocks_passage=true}
    ;
        ({_, C}, _) when (C == 0) or (C == Columns-1) ->
            #map_cell{blocks_passage=true}
    ;
        ({_, _}, _) ->
            #map_cell{blocks_passage=false}
    end,
    array_2d:map(FillFunction, UninitializedMap).

% remove_actor_from_map(Map, ActorInfo, Position)
% Removes the specified actor from the specified cell of the given map. Returns
% the result.
% If that actor is not in that cell, behavior is undefined; currently it just
% returns the original map, but that might change (for example, if we want to
% return something indicating an error).
% Map is the map.
% ActorInfo is the PID of the process storing the actor's info.
% Position is the position of the cell from which to remove the actor.
remove_actor_from_map(Map, ActorInfo, Position) ->
    % Generate the modified map_cell record.
    OldCell = array_2d:get(Position, Map),
    RemainingActors = OldCell#map_cell.actors -- [ActorInfo],
    NewCell = OldCell#map_cell{actors=RemainingActors},
    
    % Put it in the map.
    array_2d:set(Position, NewCell, Map).

% add_actor_to_map(Map, ActorInfo, Position)
% Adds the specified actor to the specified cell of the given map. Returns the
% result.
% Order of the actors in the new cell is unspecified; currently the new actor
% is put on top, but that might change.
% Map is the map.
% ActorInfo is the PID of the process storing the actor's info.
% Position is the position of the cell to which to add the actor.
add_actor_to_map(Map, ActorInfo, Position) ->
    % Generate the modified map_cell record.
    OldCell = array_2d:get(Position, Map),
    ActorsInNewCell = [ActorInfo | OldCell#map_cell.actors],
    NewCell = OldCell#map_cell{actors=ActorsInNewCell},
    
    % Put it in the map.
    array_2d:set(Position, NewCell, Map).

