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



% manage_map(Size)
% This function should be spawned as a process.
% The resulting process serves as the authority on what's at each cell of the
% map.
% 
% Makes blocking requests of various actor info processes.
manage_map(Size) ->
    Map = create_map(Size),
    manage_map_helper(Map).

manage_map_helper(Map) ->
    receive
        % Send back to Sender the size of the map, in the form {Rows, Columns}.
        {Sender, request_size, {}} ->
            Size = array_2d:size(Map),
            Sender ! {ok, {request_size, {}}, Size},
            manage_map_helper(Map)
    ;
        % Send back to Sender the record containing information on the map cell
        % at the specified Position.
        {Sender, request_cell, {Position}} ->
            Cell = array_2d:get(Position, Map),
            Sender ! {ok, {request_cell, {Position}}, Cell},
            manage_map_helper(Map)
    ;
        % Put a new actor in the map.
        % Position is the position at which to place the actor.
        % ActorInfo is the PID of the process storing information on the actor.
        {_Sender, new_actor, {Position, ActorInfo}} ->
            % The real work is done by a helper function.
            % FIXME: The helper function and the message should probably take
            % their arguments in the same order.
            NewMap = add_actor_to_map(Map, ActorInfo, Position),
            manage_map_helper(NewMap)
    ;
        % Move an actor to a new location.
        % ActorInfo is the PID of the process storing information on the actor.
        % NewPosition is the position to which to move the actor.
        {_Sender, move_actor, {ActorInfo, NewPosition}} ->
            % Figure out the actor's old position. We trust the ActorInfo
            % process to store the correct position, so just ask it.
            OldPosition = user_info_manager:get_actor_position(ActorInfo),
            
            % Remove the actor from the map, then put it back in at the new
            % location.
            MapWithoutActor = remove_actor_from_map(Map, ActorInfo,
                                                    OldPosition),
            MapWithActorMoved = add_actor_to_map(MapWithoutActor, ActorInfo,
                                                 NewPosition),
            
            % Notify the actor that it moved.
            % In general, the actor's controller should also find out about
            % this. At least for now, we'll let the actor's info process deal
            % with telling the controller.
            ActorInfo ! {self(), move_in_map, NewPosition},
            
            manage_map_helper(MapWithActorMoved)
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

