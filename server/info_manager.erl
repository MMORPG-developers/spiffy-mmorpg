-module(info_manager).

-export([
% For calling
    % ...
% For spawning
    manage_information/1
]).

-include("map_cell.hrl").



manage_information(MapManager) ->
    manage_information_helper(MapManager, dict:new()).

manage_information_helper(MapManager, TagDict) ->
    receive
        {_Sender, new_actor, {ActorInfo, Tag}} ->
            % TODO: Crash better if the tag is already in use.
            case dict:is_key(Tag, TagDict) of
                false ->
                    NewTagDict = dict:append(Tag, ActorInfo, TagDict),
                    manage_information_helper(MapManager, NewTagDict)
            end
    ;
        {Sender, request_map_all, {ObserverTag}} ->
            % FIXME: Apparently dict:fetch returns a list. Do we just pattern
            % match like this every time we do a dict:fetch?
            [ObserverInfo] = dict:fetch(ObserverTag, TagDict),
            ObserverPosition = user_info_manager:get_actor_position(
                ObserverInfo),
            VisibleCells = get_all_visible_map_cells(MapManager,
                                                     ObserverPosition),
            % TODO: More complicated calculations. Maybe the observer is blind,
            % for example.
            send_all_map_cells_info_to(Sender, VisibleCells, MapManager,
                                       ObserverPosition),
            manage_information_helper(MapManager, TagDict)
    ;
        {_Sender, action, {Tag, walk, {Direction}}} ->
            [ActorInfo] = dict:fetch(Tag, TagDict),
            {DeltaRows, DeltaColumns} = get_movement_delta(Direction),
            
            {OldRow, OldColumn} = user_info_manager:get_actor_position(
                ActorInfo),
            NewRow = OldRow + DeltaRows,
            NewColumn = OldColumn + DeltaColumns,
            NewPosition = {NewRow, NewColumn},
            
            % Check if the Actor can walk into the destination cell or not.
            DestinationCell = map:get_map_cell(MapManager, NewPosition),
            case DestinationCell#map_cell.blocks_passage of
                false ->
                    % The actor can move into that square.
                    MapManager ! {self(), move_actor, {ActorInfo,
                                  NewPosition}}
            ;
                true ->
                    % Just don't move the actor.
                    ok
            end,
            
            manage_information_helper(MapManager, TagDict)
    end.



get_movement_delta(north) ->
    {-1, 0};
get_movement_delta(east) ->
    {0, 1};
get_movement_delta(south) ->
    {1, 0};
get_movement_delta(west) ->
    {0, -1};
get_movement_delta(northeast) ->
    {-1, 1};
get_movement_delta(southeast) ->
    {1, 1};
get_movement_delta(southwest) ->
    {1, -1};
get_movement_delta(northwest) ->
    {-1, -1}.

% Returns a list of {Row, Column} pairs, each one corresponding to one map
% cell visible from the given position.
% Map is the PID of the map process.
% ObserverPosition is of the form {Row, Column}.
get_all_visible_map_cells(MapManager, _ObserverPosition) ->
    {NumberOfRows, NumberOfColumns} = map:get_map_size(MapManager),
    AllRows = lists:seq(0, NumberOfRows - 1),
    AllColumns = lists:seq(0, NumberOfColumns - 1),
    [{Row, Column} || Row <- AllRows, Column <- AllColumns].

% FIXME: Use better names.
send_all_map_cells_info_to(_Recipient, [], _MapManager, _Origin) -> ok;
send_all_map_cells_info_to(Recipient, [Position | OtherPositions],
                           MapManager, Origin) ->
    send_map_cell_info_to(Recipient, Position, MapManager, Origin),
    send_all_map_cells_info_to(Recipient, OtherPositions, MapManager, Origin).

send_map_cell_info_to(Recipient, CellPosition, MapManager, Origin) ->
    {CellRow, CellColumn} = CellPosition,
    {OriginRow, OriginColumn} = Origin,
    RelativeRow = CellRow - OriginRow,
    RelativeColumn = CellColumn - OriginColumn,
    RelativePosition = {RelativeRow, RelativeColumn},
    
    % MapCell = array_2d:get(CellPosition, Map),
    MapCell = map:get_map_cell(MapManager, CellPosition),
    Recipient ! {self(), update_map_cell, {RelativePosition, MapCell}}.

