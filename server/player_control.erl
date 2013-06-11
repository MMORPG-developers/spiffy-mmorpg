-module(player_control).

-export([
% For calling
    % ...
% For spawning
    listen_to_client/2,
    control_user/3
]).

-define(FLOOR_CODE, <<"0">>).
-define(WALL_CODE, <<"1">>).

-include("map-cell.hrl").


control_user(Socket, Tag, InfoManager) ->
    spawn(?MODULE, listen_to_client, [Socket, self()]),
    
    InfoManager ! {self(), request_map_all, {Tag}},
    
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

