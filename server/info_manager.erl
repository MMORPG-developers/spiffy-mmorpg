-module(info_manager).

-export([
    handler/4
]).

-include("map_cell.hrl").



% This function should be used as the handler of a process
% (see inter_process:main_loop/2).
% The resulting process serves as the authority on who knows what -- if any
% actor wants to know any information, they ask the information manager, who
% tells them whatever they know. (This is so that, for example, you can't get
% information about what someone's wearing if there's a wall between the two of
% you.)
% Currently, it's also responsible for executing actions (and checking whether
% they're valid).
% 
% The initial arguments to this process should be one value:
% MapManager, the PID of the map-managing process.
% 
% For subsequent iterations, the Data tuple will contain two values:
% the PID of the map-managing process and a dictionary mapping tags to
% actor info PIDs.
% 
% Note: a lot of messages this process accepts take for granted that the sender
% is using the correct tag. Since that tag will *always* originate on the
% server, we needn't worry about malicious code falsifying its tag. So as long
% as we, as developers, never decide to lie about anyone's tag, we should be
% fine.
% 
% FIXME: I'm not sure I'm really comfortable with that....
% 
% Makes blocking requests of the map manager and various actor info processes.

% Just spawned.
handler({}, setup, _, {MapManager}) ->
    % Initialize the tag assignment manager.
    TagAssignments = inter_process:spawn_with_handler(
        {tag_assignment_manager, handler}, {}),
    
    {handler_continue, {MapManager, TagAssignments}};

% Put a new (Tag, ActorInfo) pair into the dictionary.
% ActorInfo should be the PID of the process storing the actor's
% information, and Tag should be the numerical tag of the new actor.
% FIXME: Swap order of ActorInfo and Tag. I think.
handler({MapManager, TagAssignments}, notification, new_actor,
        {ActorInfo, Tag}) ->
    inter_process:send_notification(
        TagAssignments, assign_tag, {Tag, ActorInfo}),
    
    {handler_continue, {MapManager, TagAssignments}};

% Remove an actor from the game. Don't ever call this except using your own
% tag.
% FIXME: In fact, this is an easy enough cheat I'm tempted to suggest
% implementing some sort of assurance you own that tag... maybe there's a
% second magic value generated with the tag that only the owner of the tag and
% the InfoManager know?
handler({MapManager, TagAssignments}, notification, remove_actor,
        {Tag}) ->
    % Figure out who has that tag, so we can remove them from the map.
    % FIXME: The map manager should probably store tags, not PIDs.
    {ok, ActorInfo} = inter_process:make_request(
        TagAssignments, lookup_tag, {Tag}),
    
    % Remove that tag from the tag dictionary.
    inter_process:send_notification(
        TagAssignments, unassign_tag, {Tag}),
    
    % XXX: We should probably also notify the tag allocator to free
    % up the tag. But we don't have the PID of the tag allocator. That
    % was poor planning....
    
    % Remove them from the map as well.
    inter_process:send_notification(
        MapManager, remove_actor, {ActorInfo}),
    
    {handler_continue, {MapManager, TagAssignments}};

% Send back to Sender, in separate messages (one for each cell), all
% known information about the map.
% ObserverTag should be the tag of the actor requesting the
% information.
% XXX: Make this a request instead of a notification.
handler(Data = {MapManager, TagAssignments}, notification, get_map_all,
        {ObserverPid, ObserverTag}) ->
    % Look up the observer.
    {ok, ObserverInfo} = inter_process:make_request(
        TagAssignments, lookup_tag, {ObserverTag}),
    
    % For now, just do a line-of-sight calculation from their position.
    % FIXME: Do more complicated calculations. Maybe the observer is
    % blind, for example.
    {ok, ObserverPosition} = inter_process:make_request(
        ObserverInfo, get_position, {}),
    VisibleCells = get_all_visible_map_cells(MapManager,
                                             ObserverPosition),
    
    % Send them the information.
    {ok, ObserverOrigin} = inter_process:make_request(
        ObserverInfo, get_origin, {}),
    send_all_map_cells_info_to(ObserverPid, VisibleCells, MapManager,
                               ObserverOrigin),
    
    {handler_continue, Data};

% Inform the relevant actors that the cell at Position has changed.
% OldCell and NewCell should be map_cell records indicating the state
% of the cell before and after the change (respectively).
handler(Data = {_MapManager, TagAssignments}, notification, update_map_cell,
        {Position, OldCell, NewCell}) ->
    % Get a list of all actors in the world.
    {ok, AllActors} = inter_process:make_request(
        TagAssignments, get_all_actors, {}),
    
    % Send updated map cell information to the relevant actors.
    send_updated_cell_info_to_all(AllActors, Position, OldCell, NewCell),
    
    {handler_continue, Data};

% Inform the relevant actor (and possibly anyone who can see it?) that they've
% moved.
% FIXME: This should be passed the Tag, not the ActorInfo.
handler(Data = {_MapManager, _TagDict}, notification, actor_moved,
        {ActorInfo, OldPosition, NewPosition}) ->
    % Calculate the change in position.
    % XXX: Write a library for coordinate math.
    {OldRow, OldColumn} = OldPosition,
    {NewRow, NewColumn} = NewPosition,
    DeltaRows = NewRow - OldRow,
    DeltaColumns = NewColumn - OldColumn,
    DeltaPosition = {DeltaRows, DeltaColumns},
    
    % Tell the actor it moved.
    {ok, ActorController} = inter_process:make_request(
        ActorInfo, get_controller, {}),
    inter_process:send_notification(
        ActorController, move_in_map, {DeltaPosition}),
    
    {handler_continue, Data};

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
% FIXME: Move this responsibility to a separate process.
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
            inter_process:send_notification(
                MapManager, move_actor, {ActorInfo, NewPosition})
    ;
        true ->
            % The actor cannot move into that square.
            % In this case, simply do nothing.
            ok
    end,
    
    {handler_continue, Data}.



% Private/helper Functions



% get_seen_map_cell(ActorInfo, MapCell)
% Return the given MapCell (a map_cell record), as seen by the given actor.
% ActorInfo is the PID of the actor's info-managing process.
% FIXME: Won't this function eventually need to take the MapManager as an
% argument? For, like, line-of-sight reasons and such?
get_seen_map_cell(_ActorInfo, MapCell) ->
    MapCell.

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
    {ok, MapSize} = inter_process:make_request(
        MapManager, get_size, {}),
    {NumberOfRows, NumberOfColumns} = MapSize,
    
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
    {ok, MapCell} = inter_process:make_request(
        MapManager, get_cell, {CellPosition}),
    inter_process:send_notification(
        Recipient, update_map_cell, {RelativePosition, MapCell}).


% send_updated_cell_info_to_all(Actors, Position, OldCell, NewCell)
% Sends information on the map cell at the given Position to all actors in the
% list Actors who should be notified that the cell was updated (so, for
% example, don't notify the ones that can't see that cell).
% OldCell is the old state of the cell (a map_cell record).
% NewCell is the new state of the cell.
send_updated_cell_info_to_all([], _Position, _OldCell, _NewCell) -> ok;
send_updated_cell_info_to_all([Actor | OtherActors], Position, OldCell,
                              NewCell) ->
    send_updated_cell_info_to(Actor, Position, OldCell, NewCell),
    send_updated_cell_info_to_all(OtherActors, Position, OldCell, NewCell).

% send_updated_cell_info_to(Actor, Position, OldCell, NewCell)
% As above, but for a single actor instead of many.
send_updated_cell_info_to(Actor, Position, OldCell, NewCell) ->
    SeenOldCell = get_seen_map_cell(Actor, OldCell),
    SeenNewCell = get_seen_map_cell(Actor, NewCell),
    
    case SeenOldCell == SeenNewCell of
        false ->
            % If Actor can see that OldCell and NewCell differ, then tell them
            % the cell changed.
            {ok, ActorController} = inter_process:make_request(
                Actor, get_controller, {}),
            {ok, ActorOrigin} = inter_process:make_request(
                Actor, get_origin, {}),
            
            {OriginRow, OriginColumn} = ActorOrigin,
            {CellRow, CellColumn} = Position,
            RelativeRow = CellRow - OriginRow,
            RelativeColumn = CellColumn - OriginColumn,
            RelativePosition = {RelativeRow, RelativeColumn},
            
            inter_process:send_notification(
                ActorController, update_map_cell, {RelativePosition, NewCell}),
            
            ok
    ;
        true ->
            % If Actor cannot see that OldCell and NewCell differ, don't tell
            % them anything.
            ok
    end.

