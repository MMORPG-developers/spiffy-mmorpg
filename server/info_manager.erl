-module(info_manager).

-export([
% For calling
    % ...
% For spawning
    manage_information/1
]).

-include("map_cell.hrl").



% manage_information(MapManager)
% This function should be spawned as a process.
% The resulting process serves as the authority on who knows what -- if any
% actor wants to know any information, they ask the information manager, who
% tells them whatever they know. (This is so that, for example, you can't get
% information about what someone's wearing if there's a wall between the two of
% you.)
% Currently, it's also responsible for executing actions (and checking whether
% they're valid).
% FIXME: The responsibility of checking and executing actions should be
% delegated to a separate process.
% 
% Note: a lot of messages this process accepts take for granted that the sender
% is using the correct tag. Since that tag will *always* originate on the
% server, we needn't worry about malicious code falsifying its tag. So as long
% as we, as developers, never decide to lie about anyone's tag, we should be
% fine.
manage_information(MapManager) ->
    manage_information_helper(MapManager, dict:new()).

manage_information_helper(MapManager, TagDict) ->
    receive
        % Put a new (Tag, ActorInfo) pair into the dictionary.
        % ActorInfo should be the PID of the process storing the actor's
        % information, and Tag should be the numerical tag of the new actor.
        {_Sender, new_actor, {ActorInfo, Tag}} ->
            % Use a case block so that we crash if the Tag is already in use.
            % FIXME: Crash better if the tag is already in use.
            case dict:is_key(Tag, TagDict) of
                false ->
                    % Map Tag to ActorInfo in the dictionary.
                    NewTagDict = dict:append(Tag, ActorInfo, TagDict),
                    manage_information_helper(MapManager, NewTagDict)
            end
    ;
        % Send back to Sender, in separate messages (one for each cell), all
        % known information about the map.
        % ObserverTag should be the tag of the actor requesting the
        % information.
        {Sender, request_map_all, {ObserverTag}} ->
            % Look up the observer in the dictionary.
            % FIXME: Apparently dict:fetch returns a list. Do we just pattern
            % match like this every time we do a dict:fetch?
            [ObserverInfo] = dict:fetch(ObserverTag, TagDict),
            
            % For now, just to a line-of-sight calculation from their position.
            % FIXME: Do more complicated calculations. Maybe the observer is
            % blind, for example.
            ObserverPosition = user_info_manager:get_actor_position(
                ObserverInfo),
            VisibleCells = get_all_visible_map_cells(MapManager,
                                                     ObserverPosition),
            
            % --- This one ---
            % FIXME: What's to stop the recipient from moving after we
            % calculate their position but before we finish sending them the
            % map data? If that happens, then we would refer to the remaining
            % cells using incorrect relative positions. This looks like a nasty
            % race condition....
            % 
            % Ok, here's a solution. Instead of sending coordinates relative to
            % the current position, send them relative to the cell where the
            % player entered the map. Whenever we send the player a new_map
            % packet (meaning that they should forget all remembered map cells
            % because they're in a new place), we reset that "origin square" to
            % their current location. If the player enters a new map while
            % we're in the middle of sending map data, we should stop sending
            % it anyway because it's no longer valid. So we'll still need to
            % detect that, but it's a much less common and less nasty race
            % condition than the old one (and this one seems like it might be
            % unavoidable anyway).
            % 
            % For larger maps which are divided into small chunks (like the
            % main world), we'll need to establish a system of absolute
            % coordinates. I recommend that we give each submap the PID of the
            % submap storing the origin cell (position {0, 0}), so that we can
            % easily check if two submaps are part of the same larger map. Each
            % submap will also store the coordinates of its top-left corner (or
            % some such) so that we can quickly calculate the coordinates of
            % any cell relative to any actor.
            
            % Send them the information.
            ObserverOrigin = user_info_manager:get_actor_origin(
                ObserverInfo),
            send_all_map_cells_info_to(Sender, VisibleCells, MapManager,
                                       ObserverOrigin),
            manage_information_helper(MapManager, TagDict)
    ;
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
        {_Sender, action, {Tag, walk, {Direction}}} ->
            % Translate the Tag and Direction.
            [ActorInfo] = dict:fetch(Tag, TagDict),
            {DeltaRows, DeltaColumns} = get_movement_delta(Direction),
            
            % Calculate the destination location.
            {OldRow, OldColumn} = user_info_manager:get_actor_position(
                ActorInfo),
            NewRow = OldRow + DeltaRows,
            NewColumn = OldColumn + DeltaColumns,
            NewPosition = {NewRow, NewColumn},
            
            % Check if the Actor can walk into the destination cell or not.
            % Note that since walking is by definition between adjacent cells,
            % we need not check if there are cells in between.
            DestinationCell = map:get_map_cell(MapManager, NewPosition),
            case DestinationCell#map_cell.blocks_passage of
                false ->
                    % The actor can move into that square.
                    MapManager ! {self(), move_actor, {ActorInfo,
                                  NewPosition}}
            ;
                true ->
                    % The actor cannot move into that square.
                    % In this case, simply do nothing.
                    ok
            end,
            
            manage_information_helper(MapManager, TagDict)
    end.



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

% get_all_visible_map_cells(MapManager, ObserverPosition)
% Returns a list of {Row, Column} pairs, each one corresponding to one map
% cell visible from the given position.
% MapManager is the PID of the map manager process.
% ObserverPosition is the position of the observer, in the form {Row, Column}.
get_all_visible_map_cells(MapManager, _ObserverPosition) ->
    % Currently, just return a list of all cells in the map.
    % FIXME: Do some semblance of actual line-of-sight calculations.
    {NumberOfRows, NumberOfColumns} = map:get_map_size(MapManager),
    
    % Use list comprehensions to return the Cartesian product of
    % [0 .. NumberOfRows - 1] and [0 .. NumberOfColumns - 1].
    AllRows = lists:seq(0, NumberOfRows - 1),
    AllColumns = lists:seq(0, NumberOfColumns - 1),
    [{Row, Column} || Row <- AllRows, Column <- AllColumns].

% send_all_map_cells_info_to(Recipient, CellPositions, MapManager, Origin)
% Sends information for the map cells at the given positions to the given
% recipient. 
% Recipient is the PID that should receive the information.
% CellPositions is a list of {Row, Column} pairs of locations of map cells
% about which to send information.
% MapManager is the PID of the process that manages the map information.
% As information about each map cell is sent to Recipient, the cell is
% specified by its coordinates relative to the cell located at Origin.
% FIXME: This function needs a better name.
send_all_map_cells_info_to(_Recipient, [], _MapManager, _Origin) -> ok;
send_all_map_cells_info_to(Recipient, [Position | OtherPositions],
                           MapManager, Origin) ->
    send_map_cell_info_to(Recipient, Position, MapManager, Origin),
    send_all_map_cells_info_to(Recipient, OtherPositions, MapManager, Origin).

% send_map_cell_info_to(Recipient, CellPosition, MapManager, Origin)
% As above, but for a single position instead of many.
send_map_cell_info_to(Recipient, CellPosition, MapManager, Origin) ->
    % Calculate the position relative to the Origin.
    {CellRow, CellColumn} = CellPosition,
    {OriginRow, OriginColumn} = Origin,
    RelativeRow = CellRow - OriginRow,
    RelativeColumn = CellColumn - OriginColumn,
    RelativePosition = {RelativeRow, RelativeColumn},
    
    % Since we don't have any real information in MapCells yet anyway, just
    % send the whole thing for now.
    MapCell = map:get_map_cell(MapManager, CellPosition),
    Recipient ! {self(), update_map_cell, {RelativePosition, MapCell}}.

