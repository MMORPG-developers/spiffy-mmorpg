-module(existentialist_server).

% We need to export handle_connection so that spawn can find it.
-export([
% Functions actually used by outside code
    main/0,
% Functions that we spawn
    manage_tags/0,
    manage_map/1,
    manage_information/1,
    initialize_user/4,
    control_user/3,
    manage_user_info/2
]).

-define(PORT, 6667).

-define(FLOOR_CODE, <<"0">>).
-define(WALL_CODE, <<"1">>).


-record(user_info, {
    tag,
    username = "",
    % FIXME: Are we actually overlapping maps?
    maps = [],
    position
}).

-record(map_cell, {
    blocks_passage = false,
    actors = []
}).

% TODO: Have a record for the information that a particular actor knows about
% a map cell.





main() -> wait_for_connections().


wait_for_connections() ->
    TagManager = spawn(?MODULE, manage_tags, []),
    MapManager = spawn(?MODULE, manage_map, [{12, 16}]),
    InfoManager = spawn(?MODULE, manage_information, [MapManager]),
    
    {ok, ListeningSocket} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
    
    wait_for_connections_helper(ListeningSocket, TagManager, MapManager,
                                InfoManager).

wait_for_connections_helper(ListeningSocket, TagManager, MapManager,
                            InfoManager) ->
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    
    % Create a user for the next connection
    spawn(?MODULE, initialize_user, [Socket, TagManager, MapManager,
                                     InfoManager]),
    
    % Continue waiting for connections
    wait_for_connections_helper(ListeningSocket, TagManager, MapManager,
                                InfoManager).









% Having a single tag-managing thread will probably be too much of a bottleneck
% for a distributed server. However, once we reach such a large scale, we
% should be able to divide up tags by map or some such. As long as each tag
% has a prefix that is unique to its map (or server, or whatever), then
% distributed servers can generate unique tags without needing a centralized
% tag manager.
% 
% As an additional note: running out of tags that the client can handle is
% really not a concern; we should run out of PIDs long before then.

manage_tags() ->
    % Don't generate tags too small just in case it makes a difference.
    manage_tags_helper(10).

manage_tags_helper(Previous) ->
    receive
        {Sender, request_tag, {}} ->
            NewTag = Previous + 1,
            Sender ! {ok, {request_tag, {}}, NewTag},
            manage_tags_helper(NewTag)
    ;
        {_Sender, free_tag, {_Tag}} ->
            % The OS/161 solution: free is a no-op.
            % FIXME: Don't allow this in production code.
            manage_tags_helper(Previous)
    end.

get_new_tag(TagManager) ->
    TagManager ! {self(), request_tag, {}},
    receive
        {ok, {request_tag, {}}, Tag} ->
            Tag
    end.

free_tag(TagManager, Tag) ->
    TagManager ! {self(), free_tag, {Tag}}.








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
            ObserverPosition = get_actor_position(ObserverInfo),
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
            
            {OldRow, OldColumn} = get_actor_position(ActorInfo),
            NewRow = OldRow + DeltaRows,
            NewColumn = OldColumn + DeltaColumns,
            NewPosition = {NewRow, NewColumn},
            
            % FIXME: Check if the Actor can walk through the destination cell
            % or not.
            
            MapManager ! {self(), move_actor, {ActorInfo, NewPosition}},
            manage_information_helper(MapManager, TagDict)
    end.

% Returns a list of {Row, Column} pairs, each one corresponding to one map
% cell visible from the given position.
% Map is the PID of the map process.
% ObserverPosition is of the form {Row, Column}.
get_all_visible_map_cells(MapManager, _ObserverPosition) ->
    {NumberOfRows, NumberOfColumns} = get_map_size(MapManager),
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
    MapCell = get_map_cell(MapManager, CellPosition),
    Recipient ! {self(), update_map_cell, {RelativePosition, MapCell}}.








manage_map(Size) ->
    Map = create_map(Size),
    manage_map_helper(Map).

manage_map_helper(Map) ->
    receive
        {Sender, request_size, {}} ->
            Size = array_2d:size(Map),
            Sender ! {ok, {request_size, {}}, Size},
            manage_map_helper(Map)
    ;
        {Sender, request_cell, {Position}} ->
            Cell = array_2d:get(Position, Map),
            Sender ! {ok, {request_cell, {Position}}, Cell},
            manage_map_helper(Map)
    ;
        {_Sender, new_actor, {Position, ActorInfo}} ->
            % FIXME: Make argument order more consistent.
            NewMap = add_actor_to_map(Map, ActorInfo, Position),
            manage_map_helper(NewMap)
    ;
        {_Sender, move_actor, {ActorInfo, NewPosition}} ->
            OldPosition = get_actor_position(ActorInfo),
            
            MapWithoutActor = remove_actor_from_map(Map, ActorInfo,
                                                    OldPosition),
            MapWithActorMoved = add_actor_to_map(MapWithoutActor, ActorInfo,
                                                 NewPosition),
            
            % Tell the actor it moved.
            % FIXME: Should we tell the ActorInfo or the ActorController?
            ActorInfo ! {self(), move_in_map, NewPosition},
            
            manage_map_helper(MapWithActorMoved)
    end.

create_map(Size = {Rows, Columns}) ->
    UninitializedMap = array_2d:new(Size),
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

remove_actor_from_map(Map, ActorInfo, Position) ->
    OldCell = array_2d:get(Position, Map),
    RemainingActors = OldCell#map_cell.actors -- [ActorInfo],
    NewCell = OldCell#map_cell{actors=RemainingActors},
    array_2d:set(Position, NewCell, Map).

add_actor_to_map(Map, ActorInfo, Position) ->
    OldCell = array_2d:get(Position, Map),
    ActorsInNewCell = [ActorInfo | OldCell#map_cell.actors],
    NewCell = OldCell#map_cell{actors=ActorsInNewCell},
    array_2d:set(Position, NewCell, Map).

get_map_cell(MapManager, Position) ->
    MapManager ! {self(), request_cell, {Position}},
    receive
        {ok, {request_cell, {Position}}, Cell} ->
            Cell
    end.

get_map_size(MapManager) ->
    MapManager ! {self(), request_size, {}},
    receive
        {ok, {request_size, {}}, Size} ->
            Size
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




initialize_user(Socket, TagManager, MapManager, InfoManager) ->
    Tag = get_new_tag(TagManager),
    
    % FIXME: We seriously need a better way of placing users. But that might
    % have to wait until we have enough maps to actually designate a newbie
    % area....
    Position = {1, 1},
    UserInfo = #user_info{tag=Tag, maps=[MapManager], position=Position},
    
    UserController = spawn(?MODULE, control_user, [Socket, Tag, InfoManager]),
    UserInfoManager = spawn(?MODULE, manage_user_info, [UserInfo,
                                                        UserController]),
    
    InfoManager ! {self(), new_actor, {UserInfoManager, Tag}},
    MapManager ! {self(), new_actor, {Position, UserInfoManager}},
    
    % Lie about the caller because the response is supposed to go to the
    % UserController, not us.
    InfoManager ! {UserController, request_map_all, {Tag}},
    
    % TODO: Actually have the user log in.
    
    listen_to_client(Socket, UserController).







manage_user_info(UserInfo, UserController) ->
    receive
        {Sender, request_controller, {}} ->
            Sender ! {ok, {request_controller, {}}, UserController},
            manage_user_info(UserInfo, UserController)
    ;
        {Sender, request_coordinates, {}} ->
            Position = UserInfo#user_info.position,
            Sender ! {ok, {request_coordinates, {}}, Position},
            manage_user_info(UserInfo, UserController)
    ;
        {_Sender, move_in_map, {NewRow, NewColumn}} ->
            % Calculate the change.
            {OldRow, OldColumn} = UserInfo#user_info.position,
            DeltaRows = NewRow - OldRow,
            DeltaColumns = NewColumn - OldColumn,
            DeltaPosition = {DeltaRows, DeltaColumns},
            
            % Update our position.
            NewUserInfo = UserInfo#user_info{position={NewRow, NewColumn}},
            
            % Pass this on to the UserController.
            % FIXME: Should this in general be the responsibility of the
            % ActorInfo? I'm not convinced that having the controller be
            % notified based on modifications to the underlying data is a good
            % precedent to set.
            UserController ! {no_reply, move_in_map, DeltaPosition},
            
            manage_user_info(NewUserInfo, UserController)
    end.

get_actor_position(UserInfoManager) ->
    UserInfoManager ! {self(), request_coordinates, {}},
    receive
        {ok, {request_coordinates, {}}, Coordinates} ->
            Coordinates
    end.

get_actor_controller(UserInfoManager) ->
    UserInfoManager ! {self(), request_controller, {}},
    receive
        {ok, {request_controller, {}}, Controller} ->
            Controller
    end.
    





% We don't currently need the helper version of this function, but I predict
% we'll need it later.
control_user(Socket, Tag, InfoManager) ->
    control_user_helper(Socket, Tag, InfoManager).

control_user_helper(Socket, Tag, InfoManager) ->
    receive
        {_Sender, packet_from_client, {Data}} ->
            case decode_client_request(Data) of
                {action, {RequestType, RequestArguments}} ->
                    InfoManager ! {self(), action, {Tag, RequestType,
                                      RequestArguments}},
                    control_user_helper(Socket, Tag, InfoManager)
            ;
                {error, _} ->
                    io:format("Error: unable to decode client request.~n", []),
                    % FIXME: Tell the user their request failed or something.
                    % Like, actually handle the error instead of just logging
                    % it at the server end.
                    control_user_helper(Socket, Tag, InfoManager)
            end
    ;
        {_Sender, move_in_map, Delta} ->
            Binary = encode_move_in_map(Delta),
            client_connection:send(Socket, Binary),
            control_user_helper(Socket, Tag, InfoManager)
    ;
        {_Sender, update_map_cell, {RelativePosition, CellInfo}} ->
            Binary = encode_update_map_cell(RelativePosition, CellInfo),
            client_connection:send(Socket, Binary),
            control_user_helper(Socket, Tag, InfoManager)
    % TODO: the protocol should eventually support a new_map message as well.
    end.

encode_move_in_map({DeltaRows, DeltaColumns}) ->
    Separator = <<" ">>,
    Prefix = <<"move_in_map">>,
    EncodedHorizontal = list_to_binary(integer_to_list(DeltaColumns)),
    EncodedVertical = list_to_binary(integer_to_list(DeltaRows)),
    <<Prefix/binary, Separator/binary, EncodedHorizontal/binary,
        Separator/binary, EncodedVertical/binary>>.

encode_update_map_cell(RelativePosition, CellInfo) ->
    {DeltaRows, DeltaColumns} = RelativePosition,
    Separator = <<" ">>,
    Prefix = <<"update_map_cell">>,
    EncodedHorizontal = list_to_binary(integer_to_list(DeltaColumns)),
    EncodedVertical = list_to_binary(integer_to_list(DeltaRows)),
    EncodedCell = case CellInfo#map_cell.blocks_passage of
        true ->
            ?WALL_CODE
    ;
        false ->
            ?FLOOR_CODE
    end,
    % FIXME: Also list the actors in this cell.
    <<Prefix/binary, Separator/binary, EncodedHorizontal/binary,
        Separator/binary, EncodedVertical/binary, Separator/binary,
        EncodedCell/binary>>.

decode_client_request(Data) ->
    % TODO: Have a handwritten function to convert binaries to atoms that
    % only accepts valid choices? Like, one that only works on the eight
    % directions and otherwise returns error?
    case split_first_word(Data) of
        {<<"action">>, Action} ->
            case split_first_word(Action) of
                {<<"walk">>, Direction} ->
                    case Direction of
                        <<"north">> ->
                            {action, {walk, {north}}}
                    ;
                        <<"east">> ->
                            {action, {walk, {east}}}
                    ;
                        <<"south">> ->
                            {action, {walk, {south}}}
                    ;
                        <<"west">> ->
                            {action, {walk, {west}}}
                    ;
                        <<"northeast">> ->
                            {action, {walk, {northeast}}}
                    ;
                        <<"southeast">> ->
                            {action, {walk, {southeast}}}
                    ;
                        <<"southwest">> ->
                            {action, {walk, {southwest}}}
                    ;
                        <<"northwest">> ->
                            {action, {walk, {northwest}}}
                    ;
                        _ ->
                            {error, invalid_direction}
                    end
            ;
                _ ->
                    {error, invalid_command}
            end
    ;
        _ ->
            {error, invalid_request}
    end.

% Trim leading whitespace, then split at the first sequence of whitespaces.
% Returns a tuple {First, Rest} where First is the first word (everything up to
% the whitespace) and Rest is everything after the whitespace. If Binary does
% not contain any non-leading whitespace, returns {TrimmedBinary, <<>>}, where
% TrimmedBinary is Binary with all leading whitespace removed.
split_first_word(Binary) ->
    % TODO: badarg if not binary (in general, we should generate more badargs).
    case re:split(Binary, "\\s+", [{return, binary}, {parts, 2}]) of
        [Word] ->
            {Word, <<>>}
    ;
        [<<>>, Trimmed] ->
            split_first_word(Trimmed)
    ;
        [First, Rest] ->
            {First, Rest}
    end.






listen_to_client(Socket, UserController) ->
    % Get messages from the user as long as the connection is open,
    % then close the socket from our end.
    ok = listen_to_client_helper(Socket, UserController),
    gen_tcp:close(Socket).

listen_to_client_helper(Socket, UserController) ->
    case client_connection:recv(Socket) of
        % Received data from user; forward it to controller and keep going.
        {ok, Data} ->
            UserController ! {no_reply, packet_from_client, {Data}},
            listen_to_client_helper(Socket, UserController)
    ;
        % Connection closed.
        {error, closed} ->
            ok
    end.


