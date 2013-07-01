-module(map).

-export([
% For calling
    get_map_cell/2,
    get_map_size/1,
% For spawning
    manage_map/1
]).

-include("map_cell.hrl").


% get_map_cell(MapManager, Position)
% Returns the record containing information on the map cell at the specified
% Position.
% MapManager is the PID of the process that manages the map.
% Position is the position of the desired cell.
% 
% FIXME: There are a lot of functions out there that are really just "send a
% message; wait for a response." They should all be combined into a single
% generalized function to do that.
get_map_cell(MapManager, Position) ->
    % Request the cell at that position.
    MapManager ! {self(), request_cell, {Position}},
    receive
        % Wait for the response; return the cell record.
        {ok, {request_cell, {Position}}, Cell} ->
            Cell
    end.

% get_map_size(MapManager)
% Returns the size of the map.
% MapManager is the PID of the process that manages the map.
get_map_size(MapManager) ->
    % Request the size of the map.
    MapManager ! {self(), request_size, {}},
    receive
        % Wait for the response; return the size.
        {ok, {request_size, {}}, Size} ->
            Size
    end.



% manage_map(Size, InfoManager)
% This function should be spawned as a process.
% The resulting process serves as the authority on what's at each cell of the
% map.
% InfoManager is the PID of the process in charge of distributing information
% among the actors.
% 
% Makes blocking requests of various actor info processes.
manage_map(Size) ->
    % Initialize the map.
    Map = create_map(Size),
    
    % Get the PID of the InfoManager, then enter the main loop.
    receive
        {_Sender, set_info_manager, InfoManager} ->
            manage_map_helper(Map, InfoManager)
    end.

manage_map_helper(Map, InfoManager) ->
    receive
        % Send back to Sender the size of the map, in the form {Rows, Columns}.
        {Sender, request_size, {}} ->
            Size = array_2d:size(Map),
            Sender ! {ok, {request_size, {}}, Size},
            manage_map_helper(Map, InfoManager)
    ;
        % Send back to Sender the record containing information on the map cell
        % at the specified Position.
        {Sender, request_cell, {Position}} ->
            Cell = array_2d:get(Position, Map),
            Sender ! {ok, {request_cell, {Position}}, Cell},
            manage_map_helper(Map, InfoManager)
    ;
        % Put a new actor in the map.
        % Position is the position at which to place the actor.
        % ActorInfo is the PID of the process storing information on the actor.
        {_Sender, new_actor, {Position, ActorInfo}} ->
            % Grab a copy of the map cell before we remove the actor.
            CellBefore = array_2d:get(Position, Map),
            
            % The real work is done by a helper function.
            % FIXME: The helper function and the message should probably take
            % their arguments in the same order.
            NewMap = add_actor_to_map(Map, ActorInfo, Position),
            
            % Get a copy of the map cell after we've removed the actor.
            CellAfter = array_2d:get(Position, NewMap),
            
            % Inform everyone else the cell's been updated.
            InfoManager ! {self(), update_map_cell,
                           {Position, CellBefore, CellAfter}},
            
            manage_map_helper(NewMap, InfoManager)
    ;
        % Remove an actor from the map.
        % ActorInfo is the PID of the process storing information on the actor.
        {_Sender, remove_actor, {ActorInfo}} ->
            % Figure out where the actor is.
            Position = user_info_manager:get_actor_position(ActorInfo),
            
            % Grab a copy of the map cell before we remove the actor.
            CellBefore = array_2d:get(Position, Map),
            
            % Remove the actor from the cell.
            NewMap = remove_actor_from_map(Map, ActorInfo, Position),
            
            % Get a copy of the map cell after we've removed the actor.
            CellAfter = array_2d:get(Position, NewMap),
            
            % Inform everyone else the cell's been updated.
            InfoManager ! {self(), update_map_cell,
                           {Position, CellBefore, CellAfter}},
            
            % For the rest of us, life goes on.
            manage_map_helper(NewMap, InfoManager)
    ;
        % Move an actor to a new location.
        % ActorInfo is the PID of the process storing information on the actor.
        % NewPosition is the position to which to move the actor.
        {_Sender, move_actor, {ActorInfo, NewPosition}} ->
            % Figure out the actor's old position. We trust the ActorInfo
            % process to store the correct position, so just ask it.
            OldPosition = user_info_manager:get_actor_position(ActorInfo),
            
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
            ActorInfo ! {self(), move_in_map, NewPosition},
            
            % Get copies of the map_cell records for the start and end cells
            % after they've been changed to reflect the movement of the actor.
            StartCellAfter = array_2d:get(OldPosition, MapWithActorMoved),
            EndCellAfter = array_2d:get(NewPosition, MapWithActorMoved),
            
            % Notify anyone who should know that the actor moved.
            InfoManager ! {self(), actor_moved,
                           {ActorInfo, OldPosition, NewPosition}},
            
            % Notify anyone who should know that the start and end cells have
            % been updated.
            InfoManager ! {self(), update_map_cell,
                           {OldPosition, StartCellBefore, StartCellAfter}},
            InfoManager ! {self(), update_map_cell,
                           {NewPosition, EndCellBefore, EndCellAfter}},
            
            manage_map_helper(MapWithActorMoved, InfoManager)
    end.

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

