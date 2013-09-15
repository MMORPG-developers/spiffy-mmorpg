-module(player_control).

-export([
% For external use
    handler/4,
% For spawning
    listen_to_client/2
]).

% For now, when we send the client information about a map cell we send a
% single magic value indicating whether it is floor, wall, or there's someone
% there. These are the magic codes. Don't change them unless they're changed in
% the client as well. And whatever you do, don't put whitespace in them.
-define(FLOOR_CODE, <<"F">>).
-define(WALL_CODE, <<"W">>).
-define(ACTOR_CODE, <<"A">>).

-include("map_cell.hrl").


% This function should be used as the handler of a process
% (see inter_process:main_loop/2).
% The resulting process controls the player with the given Tag.
% 
% The initial arguments to this process should be four values:
% {Socket, Tag, InfoManager, CommandExecutor}.
% Socket is the socket by which it can communicate with the client.
% Tag is the tag of this player.
% InfoManager is the process that handles information distribution.
% CommandExecutor is the process that executes commands.
% 
% For subsequent iterations, the Data tuple will contain those same three
% values.
% 
% May make blocking requests of the info manager.

% Just spawned.
handler({}, setup, _,
        Arguments = {Socket, Tag, InfoManager, _CommandExecutor}) ->
    % Create a separate process to sit around listening on the socket.
    spawn(?MODULE, listen_to_client, [Socket, self()]),
    
    % As soon as we're created, look around and see what we can see.
    % FIXME: The info manager (or some such) should probably be responsible for
    % initially sending the map information to the new player.
    inter_process:send_notification(InfoManager, get_map_all, {self(), Tag}),
    
    {handler_continue, Arguments};

% Some sort of command came through the client socket.
% Decode it and handle it.
% Data is the packet from the client, as a binary.
% XXX: Instead of having all the code in handler/4, use a case and then have
% separate functions that handle the various branches. That way we don't have
% to do stupid things with the indentation to make the message command be far
% over to the right in cases like this.
handler(Data = {_Socket, Tag, _InfoManager, CommandExecutor}, notification,
                                                            packet_from_client,
        {Packet}) ->
    % Decode it.
    case decode_client_request(Packet) of
        % action indicates a request to perform an action.
        % RequestType is an atom clarifying the particular action to
        % perform.
        % RequestArguments is a tuple of arguments for that action.
        {action, {RequestType, RequestArguments}} ->
            % Pass the information on to the InfoManager, which will
            % actually execute the command.
            inter_process:send_notification(CommandExecutor, action,
                {Tag, RequestType, RequestArguments})
    ;
        % Unable to decode the request.
        {error, _} ->
            % For now, just print out an error message.
            % FIXME: Tell the player their request failed or something
            % I guess.
            io:format("Error: unable to decode client request.~n", [])
    end,
    
    {handler_continue, Data};

% Let the player know s/he's moved (within a single map).
handler(Data = {Socket, _Tag, _InfoManager, _CommandExecutor}, notification,
                                                                   move_in_map,
        {Delta}) ->
    % Encode the information and send it through the socket.
    Binary = encode_move_in_map(Delta),
    client_connection:send(Socket, Binary),
    
    {handler_continue, Data};

% Let the player know about new information regarding some cell of the map.
handler(Data = {Socket, _Tag, _InfoManager, _CommandExecutor}, notification,
                                                               update_map_cell,
        {RelativePosition, CellInfo}) ->
    % Encode the information and send it through the socket.
    Binary = encode_update_map_cell(RelativePosition, CellInfo),
    client_connection:send(Socket, Binary),
    
    {handler_continue, Data};

% TODO: the protocol should eventually support a new_map message as well.
% But in that case, we'll need to figure out whose responsibility it is to
% send all information about the new map.

% Inform the relevant other processes that we're disconnecting from the
% server, then end this process.
handler(Data = {_Socket, Tag, InfoManager, _CommandExecutor}, notification,
                                                                       cleanup,
        {}) ->
    % The InfoManager currently takes care of all cleanup.
    inter_process:send_notification(InfoManager, remove_actor, {Tag}),
    
    % But we should still end this process.
    {handler_end, Data}.



% listen_to_client(Socket, PlayerController)
% This function should be spawned as a process.
% The resulting process listens to Socket, forwarding all incoming data to
% PlayerController.
listen_to_client(Socket, PlayerController) ->
    % Get messages from the player as long as the connection is open,
    % then close the socket from our end.
    listen_to_client_helper(Socket, PlayerController),
    
    % Once the player disconnects, close our end of the socket and clean up our
    % character.
    inter_process:send_notification(PlayerController, cleanup, {}),
    gen_tcp:close(Socket).

listen_to_client_helper(Socket, PlayerController) ->
    case client_connection:recv(Socket) of
        % Received data from player; forward it to controller and keep going.
        {ok, Packet} ->
            inter_process:send_notification(
                PlayerController, packet_from_client, {Packet}),
            listen_to_client_helper(Socket, PlayerController)
    ;
        % Connection closed.
        {error, closed} ->
            ok
    end.



% Private / helper functions



% encode_move_in_map(Displacement)
% Encodes (as a binary, ready to be sent over the socket) the message that the
% player has moved by a certain amount within their current map.
% Displacement is the amount by which the player has moved, in the form
% {DeltaRows, DeltaColumns}.
encode_move_in_map({DeltaRows, DeltaColumns}) ->
    % A couple constants we'll need.
    Separator = <<" ">>,
    Prefix = <<"move_in_map">>,
    
    % Encode the displacement.
    EncodedHorizontal = list_to_binary(integer_to_list(DeltaColumns)),
    EncodedVertical = list_to_binary(integer_to_list(DeltaRows)),
    
    % Splice it all together.
    <<Prefix/binary, Separator/binary, EncodedHorizontal/binary,
        Separator/binary, EncodedVertical/binary>>.

% encode_update_map_cell(RelativePosition, CellInfo)
% Encodes (as a binary, ready to be sent over the socket) the message that the
% player has received new information about a nearby map cell.
% RelativePosition is the position of that map cell relative to the player.
% CellInfo is a map_cell record containing all known information about that
% cell.
encode_update_map_cell(RelativePosition, CellInfo) ->
    {DeltaRows, DeltaColumns} = RelativePosition,
    
    % A couple constants we'll need.
    Separator = <<" ">>,
    Prefix = <<"update_map_cell">>,
    
    % Encode the (relative) position of the cell.
    EncodedHorizontal = list_to_binary(integer_to_list(DeltaColumns)),
    EncodedVertical = list_to_binary(integer_to_list(DeltaRows)),
    
    % Encode the contents of the cell.
    EncodedCell = encode_map_cell(CellInfo),
    
    % Splice it all together.
    <<Prefix/binary, Separator/binary, EncodedHorizontal/binary,
        Separator/binary, EncodedVertical/binary, Separator/binary,
        EncodedCell/binary>>.

% encode_map_cell(CellInfo)
% Encodes a map cell as a binary, ready to be sent over the socket.
% CellInfo is a map_cell record containing all known information about that
% cell.
encode_map_cell(CellInfo) ->
    % Currently, we simply send one of three magic values indicating whether
    % the cell is a floor, a wall, or a player.
    case CellInfo#map_cell.actors of
        [_FirstActor | _OtherActors] ->
            ?ACTOR_CODE
    ;
        [] ->
            case CellInfo#map_cell.blocks_passage of
                true ->
                    ?WALL_CODE
            ;
                false ->
                    ?FLOOR_CODE
            end
    end.

% decode_client_request(Packet)
% Parses a data packet from the client and turns it into a tuple representing
% the request using atoms. If unable to parse the packet, returns {error, Type}
% where Type is an atom indicating the type of error that occurred.
% Packet is the packet from the client, as a binary.
decode_client_request(Packet) ->
    % Parse it one word at a time.
    case split_first_word(Packet) of
        % action -- indicates a desire to perform some action. Most commands
        % that affect the physical world fall under this category.
        {<<"action">>, Action} ->
            case split_first_word(Action) of
                % walk -- moves the player a single square in one direction.
                % Fails if that square is blocked.
                {<<"walk">>, Direction} ->
                    % Convert the direction to an atom.
                    case decode_direction(Direction) of
                        {ok, DirectionAtom} ->
                            {action, {walk, {DirectionAtom}}}
                    ;
                        % Unable to parse the direction.
                        {error, _} ->
                            {error, invalid_direction}
                    end
            ;
                % Unrecognized type of action.
                _ ->
                    {error, invalid_command}
            end
    ;
        % Unrecognized type of request.
        _ ->
            {error, invalid_request}
    end.

% decode_direction(DirectionBinary)
% Parses a binary encoding of a direction and returns the corresponding atom.
% Return format: {ok, Direction} (where Direction is the atom for the relevant
% direction) if able to parse; otherwise {error, invalid_direction}.
% DirectionBinary is the binary encoding of the direction.
decode_direction(<<"north">>) -> {ok, north};
decode_direction(<<"east">>) -> {ok, east};
decode_direction(<<"south">>) -> {ok, south};
decode_direction(<<"west">>) -> {ok, west};
decode_direction(<<"northeast">>) -> {ok, northeast};
decode_direction(<<"southeast">>) -> {ok, southeast};
decode_direction(<<"southwest">>) -> {ok, southwest};
decode_direction(<<"northwest">>) -> {ok, northwest};
decode_direction(_) -> {error, invalid_direction}.


% split_first_word(Binary)
% Trim leading whitespace, then split at the first sequence of whitespaces.
% Returns a tuple {First, Rest} where First is the first word (everything up to
% the whitespace) and Rest is everything after the whitespace. If Binary does
% not contain any non-leading whitespace, returns {TrimmedBinary, <<>>}, where
% TrimmedBinary is Binary with all leading whitespace removed.
split_first_word(Binary) ->
    % FIXME: if Binary is not of type binary (or something else we can handle,
    % like perhaps an Erlang string), then indicate a badarg. (In general, we
    % should generate more badargs.)
    
    % re:split does most of the work for us. Split Binary on nonempty strings
    % of whitespace, into at most 2 parts.
    case re:split(Binary, "\\s+", [{return, binary}, {parts, 2}]) of
        % Only one part -- no whitespace to split on.
        [Word] ->
            {Word, <<>>}
    ;
        % Leading whitespace. Remove it and recurse on the rest.
        [<<>>, Trimmed] ->
            split_first_word(Trimmed)
    ;
        % Found whitespace to split on.
        [First, Rest] ->
            {First, Rest}
    end.

