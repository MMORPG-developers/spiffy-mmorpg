-module(player_control).

-export([
% For calling
    % ...
% For spawning
    listen_to_client/2,
    control_user/3
]).

% For now, when we send the client information about a map cell we send a
% single code indicating whether it is floor or wall. These are the magic
% codes. Don't change them unless they're changed in the client as well. And
% whatever you do, don't put whitespace in them.
-define(FLOOR_CODE, <<"0">>).
-define(WALL_CODE, <<"1">>).

-include("map_cell.hrl").


% control_user(Socket, Tag, InfoManager)
% This function should be spawned as a process.
% The resulting process controls the player with the given Tag.
% Socket is the socket by which it can communicate with the client.
% InfoManager is the process that handles information distribution.
control_user(Socket, Tag, InfoManager) ->
    % Create a separate process to sit around listening on the socket.
    spawn(?MODULE, listen_to_client, [Socket, self()]),
    
    % As soon as we're created, look around and see what we can see.
    InfoManager ! {self(), request_map_all, {Tag}},
    
    control_user_helper(Socket, Tag, InfoManager).

control_user_helper(Socket, Tag, InfoManager) ->
    receive
        % Some sort of command came through the client socket.
        % Decode it and handle it.
        % Data is the packet from the client, as a binary.
        {_Sender, packet_from_client, {Data}} ->
            % Decode it.
            case decode_client_request(Data) of
                % action indicates a request to perform an action.
                % RequestType is an atom clarifying the particular action to
                % perform.
                % RequestArguments is a tuple of arguments for that action.
                {action, {RequestType, RequestArguments}} ->
                    % Pass the information on to the InfoManager, which will
                    % actually execute the command.
                    InfoManager ! {self(), action, {Tag, RequestType,
                                      RequestArguments}},
                    control_user_helper(Socket, Tag, InfoManager)
            ;
                % Unable to decode the request.
                {error, _} ->
                    % For now, just print out an error message.
                    % FIXME: Tell the user their request failed or something I
                    % guess.
                    io:format("Error: unable to decode client request.~n", []),
                    control_user_helper(Socket, Tag, InfoManager)
            end
    ;
        % Let the player know s/he's moved (within a single map).
        {_Sender, move_in_map, Delta} ->
            % Encode the information and send it through the socket.
            Binary = encode_move_in_map(Delta),
            client_connection:send(Socket, Binary),
            control_user_helper(Socket, Tag, InfoManager)
    ;
        % Let the player know about new information regarding some cell of the
        % map.
        {_Sender, update_map_cell, {RelativePosition, CellInfo}} ->
            % Encode the information and send it through the socket.
            Binary = encode_update_map_cell(RelativePosition, CellInfo),
            client_connection:send(Socket, Binary),
            control_user_helper(Socket, Tag, InfoManager)
    % TODO: the protocol should eventually support a new_map message as well.
    % But in that case, we'll need to figure out whose responsibility it is to
    % send all information about the new map.
    end.


% listen_to_client(Socket, UserController)
% This function should be spawned as a process.
% The resulting process listens to Socket, forwarding all incoming data to
% UserController.
listen_to_client(Socket, UserController) ->
    % Get messages from the user as long as the connection is open,
    % then close the socket from our end.
    ok = listen_to_client_helper(Socket, UserController),
    gen_tcp:close(Socket).

listen_to_client_helper(Socket, UserController) ->
    case client_connection:recv(Socket) of
        % Received data from user; forward it to controller and keep going.
        {ok, Data} ->
            % Deliberately spoof the sender because we know we can't accept
            % messages anyway, so we might as well fail loudly.
            UserController ! {no_reply, packet_from_client, {Data}},
            listen_to_client_helper(Socket, UserController)
    ;
        % Connection closed.
        {error, closed} ->
            ok
    end.



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
    
    % Encode the contents of the cell. Currently, we simply send one of two
    % magic values indicating whether the cell is a floor or a wall.
    % FIXME: Also list the actors in the cell.
    EncodedCell = case CellInfo#map_cell.blocks_passage of
        true ->
            ?WALL_CODE
    ;
        false ->
            ?FLOOR_CODE
    end,
    
    % Splice it all together.
    <<Prefix/binary, Separator/binary, EncodedHorizontal/binary,
        Separator/binary, EncodedVertical/binary, Separator/binary,
        EncodedCell/binary>>.

% decode_client_request(Data)
% Parses a data packet from the client and turns it into a tuple representing
% the request using atoms. If unable to parse the packet, returns {error, Type}
% where Type is an atom indicating the type of error that occurred.
% Data is the packet from the client, as a binary.
decode_client_request(Data) ->
    % Parse it one word at a time.
    case split_first_word(Data) of
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

