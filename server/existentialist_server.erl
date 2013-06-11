-module(existentialist_server).

-export([
% Functions actually used by outside code
    main/0,
% Functions that we spawn
    manage_information/1,
    initialize_user/4,
    control_user/3
]).

-define(PORT, 6667).

-define(FLOOR_CODE, <<"0">>).
-define(WALL_CODE, <<"1">>).

-include("map-cell.hrl").
-include("user_info.hrl").






main() -> wait_for_connections().


wait_for_connections() ->
    TagManager = spawn(tag, manage_tags, []),
    MapManager = spawn(map, manage_map, [{12, 16}]),
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













% -include("map-cell.hrl")

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



% -include("user_info.hrl").

initialize_user(Socket, TagManager, MapManager, InfoManager) ->
    Tag = tag:get_new_tag(TagManager),
    
    % FIXME: We seriously need a better way of placing users. But that might
    % have to wait until we have enough maps to actually designate a newbie
    % area....
    Position = {1, 1},
    UserInfo = #user_info{tag=Tag, maps=[MapManager], position=Position},
    
    UserController = spawn(?MODULE, control_user, [Socket, Tag, InfoManager]),
    UserInfoManager = spawn(user_info_manager, manage_user_info,
                            [UserInfo, UserController]),
    
    InfoManager ! {self(), new_actor, {UserInfoManager, Tag}},
    MapManager ! {self(), new_actor, {Position, UserInfoManager}},
    
    % Lie about the caller because the response is supposed to go to the
    % UserController, not us.
    InfoManager ! {UserController, request_map_all, {Tag}},
    
    % TODO: Actually have the user log in.
    
    listen_to_client(Socket, UserController).










% -include("map-cell.hrl")

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


