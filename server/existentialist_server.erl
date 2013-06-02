% TODO
% Look, just get it working as far as sending the initial map data. After
% testing that, worry about the main loop. And while debugging, replace the
% `receive ... end`s with `receive Msg -> case Msg of ... end end`s, so that
% things crash when given messages they can't handle.
% TODO














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
% -define(SEND_MAP, 1).
% -define(REQUEST_MAP, 1).

-define(FLOOR_CODE, <<"0">>).
-define(WALL_CODE, <<"1">>).


-record(user_info, {
    tag,
    username = "",
    % TODO: Are we actually overlapping maps?
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










% FIXME: Will having a single tag-managing thread create too much of a
% bottleneck for a distributed server? Is a more distributed solution possible?
% (Maybe having tags be local to maps? But then we'd have to change tags
% constantly, and that would get annoying to the client and everyone else.
% I guess we could allocate different blocks to different threads...)
%
% Answer: No; it's fine. We can divide up tags by maps or some such -- as long
% as the prefix is something unique to each map, distributed servers can
% generate unique tags without needing a centralized tag allocator.
% 
% As a note: running out of tags is really not a concern; we should run out of
% PIDs long before then.

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
            % FIXME: Wait, shouldn't we just send them the updated squares with
            % no context?
            % Okay, we should be doing that....
            % Sender ! {ok, {request_map_all, {ObserverTag}}, VisibleCells},
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
            
            MapManager ! {self(), move_actor, {ActorInfo, NewPosition}},
            manage_information_helper(MapManager, TagDict)
    end.

% Returns a list of {Row, Column} pairs, each one corresponding to one map
% cell visible from the given position.
% Map is the PID of the map process.
% ObserverPosition is of the form {Row, Column}.
get_all_visible_map_cells(_MapManager, ObserverPosition) ->
    % FIXME: Look, this is obviously placeholder code. There's a lot of
    % infrastructure I want to get working before I try to implement a double
    % for loop in a functional language.
    [ObserverPosition].

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








% FIXME: How do you walk? Whom do you tell? The map can't turn your tag to an
% ActorInfo PID anymore, and if the map sends a message with callback to the
% info manager, we could get deadlock.
% 
% I think you tell the controller. It figures out your ActorInfo, then your
% Map, then tells the Map to move your ActorInfo by the appropriate Delta.
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
            % ActorInfo = dict:fetch(ActorController, ActorDict),
            % {DeltaRows, DeltaColumns} = get_movement_delta(Direction),
            % 
            % OldPosition = {OldRow, OldColumn} = get_actor_position(ActorInfo),
            % NewRow = OldRow + DeltaRows,
            % NewColumn = OldColumn + DeltaColumns,
            % NewPosition = {NewRow, NewColumn},
            
            % OldCell = array_2d:get({OldRow, OldColumn}, Map),
            % NewCell = array_2d:get({NewRow, NewColumn}, Map),
            
            % RemainingActorsInOldCell = OldCell#map_cell.actors -- ActorInfo,
            % OldCellWithoutActor = OldCell#map_cell{
            %     actors=RemainingActorsInOldCell
            % },
            % ActorsInNewCell = [ActorInfo | NewCell#map_cell.actors],
            % NewCellWithActor = NewCell#map_cell{
            %     actors=ActorsInNewCell
            % },
            
            % MapWithoutActor = array_2d:set({OldRow, OldColumn},
            %                                OldCellWithoutActor, Map),
            % MapWithActorMoved = array_2d:set({NewRow, NewColumn},
            %                                  NewCellWithActor, Map),
            
            OldPosition = get_actor_position(ActorInfo),
            
            MapWithoutActor = remove_actor_from_map(Map, ActorInfo,
                                                    OldPosition),
            MapWithActorMoved = add_actor_to_map(MapWithoutActor, ActorInfo,
                                                 NewPosition),
            
            % Tell the actor it moved.
            % FIXME: Is this the right one to tell?
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
    {-1, -1};
get_movement_delta(northwest) ->
    {1, -1}.




initialize_user(Socket, TagManager, MapManager, InfoManager) ->
    Tag = get_new_tag(TagManager),
    
    % FIXME: We seriously need a better way of placing users.
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
            % FIXME: It's not clear that moving an Actor and telling that Actor
            % it moved should always happen together.
            UserController ! {no_reply, move_in_map, DeltaPosition},
            
            % TODO: Who updates our map?
            % Wait, doesn't this message come from the map?
            % TODO: Check whether this is a problem...
            
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
                    % Do nothing. Because this is the best response to errors!
                    % FIXME: Tell the user their request failed or something I
                    % guess.
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
    % TODO: Add a case for when the map says "new map" or "new info for map
    % cell."
    end.

encode_move_in_map({DeltaRows, DeltaColumns}) ->
    % FIXME: Tired; fix later.
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

split_first_word(Binary) ->
    % TODO: badarg if not binary
    % Trim leading whitespace, then split at the first sequence of whitespaces.
    % Returns a tuple {First, Rest} where First is the first word (everything
    % up to the whitespace) and Rest is everything after the whitespace.
    % If Binary does not contain any non-leading whitespace, returns
    % {TrimmedBinary, <<>>}, where TrimmedBinary is Binary with all leading
    % whitespace removed.
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
            io:format("Received packet from client: ~s~n", [Data]),
            UserController ! {no_reply, packet_from_client, {Data}},
            listen_to_client_helper(Socket, UserController)
    ;
        % Connection closed.
        {error, closed} ->
            ok
    end.










 % -------------------------------------------------------------------------- %





% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% % Main (driver) thread
% 
% main() -> listen_for_connections().
% 
% 
% listen_for_connections() ->
%     UserManager = spawn(?MODULE, manage_users, []),
%     Map = spawn(?MODULE, manage_map, [12, 16]),
%     
%     % Open a listening socket and wait for connections
%     {ok, ListeningSocket} =
%         gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
%     listen_for_connections_helper(ListeningSocket, UserManager, Map).
% 
% listen_for_connections_helper(ListeningSocket, UserManager, Map) ->
%     % Create a user for the next connection
%     {ok, Socket} = gen_tcp:accept(ListeningSocket),
%     spawn(?MODULE, handshake_with_user, [Socket, UserManager, Map]),
%     
%     % Continue waiting for connections
%     listen_for_connections_helper(ListeningSocket, UserManager, Map).
% 
% 
% 
% % Handshaker
% 
% handshake_with_user(Socket, UserManager, Map) ->
%     % Create a state-holding process for the user with a dummy name.
%     User = spawn(?MODULE, maintain_user_state, [Socket, Map]),
%     
%     UserManager ! {self(), new_user, {User}},
%     
%     ask_user_for_name(User, Socket, UserManager).
% 
% 
% ask_user_for_name(User, Socket, UserManager) ->
%     % Get a name from the user
%     {ok, Data} = socket:recv(Socket),
%     Name = strip_whitespace(binary_to_list(Data)),
%     
%     % Try to change the user's name
%     UserManager ! {self(), change_name, {User, Name}},
%     
%     receive
%         % Success!
%         {ok, {change_name, {User, Name}}} ->
%             % Start listening for messages from this user.
%             receive_messages(User, Socket, UserManager)
%     ;
%         % The name is taken.
%         {error, name_in_use, {change_name, {User, Name}}} ->
%             % Inform the user that name is taken; ask them for another one.
%             % FIXME: Look, this is terrible. I know it, you know it, my dog
%             % knows it. Let's just move on, get a drink, and we'll fix it
%             % later.
%             ok = gen_tcp:send(Socket, "That name is already in use.\n"),
%             ask_user_for_name(User, Socket, UserManager)
%     end.
% 
% 
% 
% % State holder
% 
% maintain_user_state(Socket, Map) ->
%     maintain_user_state_helper({user, "", Socket, Map}).
% 
% 
% maintain_user_state_helper(User = {user, Name, Socket, Map}) ->
%     receive
%         % Someone asked us for our name.
%         {Caller, get_name, {}} ->
%             Caller ! {ok, {get_name, {}}, {Name}},
%             maintain_user_state_helper(User)
%     ;
%         {Caller, get_map, {}} ->
%             Caller ! {ok, {get_map, {}}, {Map}},
%             maintain_user_state_helper(User)
%     ;
%         {Caller, get_socket, {}} ->
%             Caller ! {ok, {get_socket, {}}, {Socket}},
%             maintain_user_state_helper(User)
%     ;
%         % Someone told us to take a new name.
%         {_Caller, change_name, {NewName}} ->
%             maintain_user_state_helper({user, NewName, Socket, Map})
%     end.
% 
% 
% get_name(User) ->
%     % Ask the user for their name; return their response.
%     User ! {self(), get_name, {}},
%     receive
%         {ok, {get_name, {}}, {Name}} -> Name
%     end.
% 
% get_map_containing_user(User) ->
%     % Ask the user for their map; return their response.
%     User ! {self(), get_map, {}},
%     receive
%         {ok, {get_map, {}}, {Map}} -> Map
%     end.
% 
% get_socket_for_user(User) ->
%     % Ask the user for their socket; return their response.
%     User ! {self(), get_socket, {}},
%     receive
%         {ok, {get_socket, {}}, {Socket}} -> Socket
%     end.
% 
% 
% 
% % Listener
% 
% % FIXME: Rename?
% receive_messages(User, Socket, UserManager) ->
%     % Get messages from the user as long as the connection is open,
%     % then close the socket from our end.
%     ok = receive_messages_helper(User, Socket),
%     ok = gen_tcp:close(Socket),
%     
%     % Tell the user manager that this user disconnected.
%     UserManager ! {self(), disconnection, {User}}.
% 
% 
% receive_messages_helper(User, Socket) ->
%     case socket:recv(Socket) of
%         % Received a chat from the user.
%         {ok, Data} ->
%             Request = binary:decode_unsigned(Data, big),
%             case Request of
%                 ?REQUEST_MAP ->
%                     Map = get_map_containing_user(User),
%                     respond_to_map_request(User, Map)
%             end,
%             receive_messages_helper(User, Socket)
%     ;
%         % Connection closed.
%         {error, closed} ->
%             ok
%     end.
% 
% respond_to_map_request(User, Map) ->
%     Socket = get_socket_for_user(User),
%     MapSquares = get_all_squares_in_map(Map),
%     {Rows, Columns} = array_2d:size(MapSquares),
%     ok = gen_tcp:send(Socket, <<?SEND_MAP:8>>),
%     ok = gen_tcp:send(Socket, <<Rows:32>>),
%     ok = gen_tcp:send(Socket, <<Columns:32>>),
%     send_map(Socket, MapSquares).
% 
% send_map(Socket, Map) ->
%     F = fun({_, _}, {map_cell, Type}) ->
%         case Type of
%             floor ->
%                 % FIXME: Magic
%                 gen_tcp:send(Socket, <<?FLOOR_CODE:8>>)
%         ;
%             wall ->
%                 gen_tcp:send(Socket, <<?WALL_CODE:8>>)
%         end
%     end,
%     array_2d:map(F, Map).
% 
% 
% % User manager
% 
% manage_users() -> manage_users_helper([]).
% 
% 
% manage_users_helper(Users) ->
%     receive
%         % Add another user to the list.
%         {_Caller, new_user, {User}} ->
%             manage_users_helper([User|Users])
%     ;
%         % A user requested a name change.
%         {Caller, change_name, {User, NewName}} ->
%             case is_name_in_use(NewName, Users) of
%                 % The new name isn't in use; take it.
%                 false ->
%                     User ! {self(), change_name, {NewName}},
%                     Caller ! {ok,
%                         {change_name, {User, NewName}}
%                     },
%                     manage_users_helper(Users)
%             ;
%                 % The new name is already in use; tell the caller.
%                 true ->
%                     Caller ! {error, name_in_use,
%                         {change_name, {User, NewName}}
%                     },
%                     manage_users_helper(Users)
%             end
%     ;
%         % Remove a user from the list.
%         {_Caller, disconnection, {User}} ->
%             manage_users_helper(Users -- [User])
%     end.
% 
% 
% is_name_in_use(_Name, []) -> false;
% is_name_in_use(Name, [User|OtherUsers]) ->
%     case get_name(User) of
%         % User is named Name; Name is in use.
%         Name ->
%             true
%     ;
%         % User is not named Name; see if anyone else is using that name.
%         OtherName when OtherName /= Name ->
%             is_name_in_use(Name, OtherUsers)
%     end.
% 
% % Map
% 
% manage_map(Rows, Columns) ->
%     Map = create_map(Rows, Columns),
%     manage_map_helper(Map).
% 
% manage_map_helper(Map) ->
%     receive
%         {Caller, get_map, {}} ->
%             Caller ! {ok, {get_map, {}}, {Map}},
%             manage_map_helper(Map)
%     end.
% 
% create_map(Rows, Columns) ->
%     UninitializedMap = array_2d:new({Rows, Columns}),
%     FillFunction = fun
%         ({R, _}, _) when (R == 0) or (R == Rows-1) ->
%             {map_cell, wall}
%     ;
%         ({_, C}, _) when (C == 0) or (C == Columns-1) ->
%             {map_cell, wall}
%     ;
%         ({_, _}, _) ->
%             {map_cell, floor}
%     end,
%     array_2d:map(FillFunction, UninitializedMap).
% 
% get_all_squares_in_map(Map) ->
%     % Ask the map for its squares; return their response.
%     Map ! {self(), get_map, {}},
%     receive
%         {ok, {get_map, {}}, {MapSquares}} -> MapSquares
%     end.
% 
% 
% 
% 
% 
% 
% 
% 
% % Miscellaneous utilities
% 
% % strip_whitespace(String, Direction):
% %   Removes all whitespace from one or both ends of String.
% %   Choices for Direction are: left, right, both.
% %
% % Explanation: the built-in function string:strip only strips space characters
% % (' '), not other whitespace (tabs and newlines).
% % I guess this is probably related to the fact that Erlang isn't exactly the
% % best language for string processing.
% % Credit to
% %   http://stackoverflow.com/questions/12794358/how-to-strip-all-blank-characters-in-a-string-in-erlang
% % for the regular expressions.
% strip_whitespace(String, left) ->
%     re:replace(String, "^\\s+", "", [global,{return,list}]);
% strip_whitespace(String, right) ->
%     re:replace(String, "\\s+$", "", [global,{return,list}]);
% strip_whitespace(String, both) ->
%     LeftStripped = strip_whitespace(String, left),
%     strip_whitespace(LeftStripped, right).
% 
% strip_whitespace(String) -> strip_whitespace(String, both).
% 
