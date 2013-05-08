-module(mapserver).

% We need to export handle_connection so that spawn can find it.
-export([
% Functions actually used by outside code
    main/0,
% Functions that we spawn
    manage_users/0,
    manage_map/2,
    handshake_with_user/3,
    maintain_user_state/2
]).

-define(PORT, 6667).
% -define(BYTES_PER_REQUEST, 1).
-define(SEND_MAP, 1).
-define(REQUEST_MAP, 1).

-define(FLOOR_CODE, 1).
-define(WALL_CODE, 2).














%
% User struct: {user, Name}
%



% Main (driver) thread

main() -> listen_for_connections().


listen_for_connections() ->
    UserManager = spawn(?MODULE, manage_users, []),
    Map = spawn(?MODULE, manage_map, [12, 16]),
    
    % Open a listening socket and wait for connections
    {ok, ListeningSocket} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
    listen_for_connections_helper(ListeningSocket, UserManager, Map).


listen_for_connections_helper(ListeningSocket, UserManager, Map) ->
    % Create a user for the next connection
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    spawn(?MODULE, handshake_with_user, [Socket, UserManager, Map]),
    
    % Continue waiting for connections
    listen_for_connections_helper(ListeningSocket, UserManager, Map).



% Handshaker

handshake_with_user(Socket, UserManager, Map) ->
    % Create a state-holding process for the user with a dummy name.
    User = spawn(?MODULE, maintain_user_state, [Socket, Map]),
    
    UserManager ! {self(), new_user, {User}},
    
    ask_user_for_name(User, Socket, UserManager).


ask_user_for_name(User, Socket, UserManager) ->
    % Get a name from the user
    {ok, Data} = socket:recv(Socket),
    Name = strip_whitespace(binary_to_list(Data)),
    
    % Try to change the user's name
    UserManager ! {self(), change_name, {User, Name}},
    
    receive
        % Success!
        {ok, {change_name, {User, Name}}} ->
            % Start listening for messages from this user.
            receive_messages(User, Socket, UserManager)
    ;
        % The name is taken.
        {error, name_in_use, {change_name, {User, Name}}} ->
            % Inform the user that name is taken; ask them for another one.
            % FIXME: Look, this is terrible. I know it, you know it, my dog
            % knows it. Let's just move on, get a drink, and we'll fix it
            % later.
            ok = gen_tcp:send(Socket, "That name is already in use.\n"),
            ask_user_for_name(User, Socket, UserManager)
    end.



% State holder

maintain_user_state(Socket, Map) ->
    maintain_user_state_helper({user, "", Socket, Map}).


maintain_user_state_helper(User = {user, Name, Socket, Map}) ->
    receive
        % Someone asked us for our name.
        {Caller, get_name, {}} ->
            Caller ! {ok, {get_name, {}}, {Name}},
            maintain_user_state_helper(User)
    ;
        {Caller, get_map, {}} ->
            Caller ! {ok, {get_map, {}}, {Map}},
            maintain_user_state_helper(User)
    ;
        {Caller, get_socket, {}} ->
            Caller ! {ok, {get_socket, {}}, {Socket}},
            maintain_user_state_helper(User)
    ;
        % Someone told us to take a new name.
        {_Caller, change_name, {NewName}} ->
            maintain_user_state_helper({user, NewName, Socket, Map})
    end.


get_name(User) ->
    % Ask the user for their name; return their response.
    User ! {self(), get_name, {}},
    receive
        {ok, {get_name, {}}, {Name}} -> Name
    end.

get_map_containing_user(User) ->
    % Ask the user for their map; return their response.
    User ! {self(), get_map, {}},
    receive
        {ok, {get_map, {}}, {Map}} -> Map
    end.

get_socket_for_user(User) ->
    % Ask the user for their socket; return their response.
    User ! {self(), get_socket, {}},
    receive
        {ok, {get_socket, {}}, {Socket}} -> Socket
    end.



% Listener

% FIXME: Rename?
receive_messages(User, Socket, UserManager) ->
    % Get messages from the user as long as the connection is open,
    % then close the socket from our end.
    ok = receive_messages_helper(User, Socket),
    ok = gen_tcp:close(Socket),
    
    % Tell the user manager that this user disconnected.
    UserManager ! {self(), disconnection, {User}}.


receive_messages_helper(User, Socket) ->
    case socket:recv(Socket) of
        % Received a chat from the user.
        {ok, Data} ->
            Request = binary:decode_unsigned(Data, big),
            case Request of
                ?REQUEST_MAP ->
                    Map = get_map_containing_user(User),
                    respond_to_map_request(User, Map)
            end,
            receive_messages_helper(User, Socket)
    ;
        % Connection closed.
        {error, closed} ->
            ok
    end.

respond_to_map_request(User, Map) ->
    Socket = get_socket_for_user(User),
    MapSquares = get_all_squares_in_map(Map),
    {Rows, Columns} = array_2d:size(MapSquares),
    ok = gen_tcp:send(Socket, <<?SEND_MAP:8>>),
    ok = gen_tcp:send(Socket, <<Rows:32>>),
    ok = gen_tcp:send(Socket, <<Columns:32>>),
    send_map(Socket, MapSquares).

send_map(Socket, Map) ->
    F = fun({_, _}, {map_cell, Type}) ->
        case Type of
            floor ->
                % FIXME: Magic
                gen_tcp:send(Socket, <<?FLOOR_CODE:8>>)
        ;
            wall ->
                gen_tcp:send(Socket, <<?WALL_CODE:8>>)
        end
    end,
    array_2d:map(F, Map).


% User manager

manage_users() -> manage_users_helper([]).


manage_users_helper(Users) ->
    receive
        % Add another user to the list.
        {_Caller, new_user, {User}} ->
            manage_users_helper([User|Users])
    ;
        % A user requested a name change.
        {Caller, change_name, {User, NewName}} ->
            case is_name_in_use(NewName, Users) of
                % The new name isn't in use; take it.
                false ->
                    User ! {self(), change_name, {NewName}},
                    Caller ! {ok,
                        {change_name, {User, NewName}}
                    },
                    manage_users_helper(Users)
            ;
                % The new name is already in use; tell the caller.
                true ->
                    Caller ! {error, name_in_use,
                        {change_name, {User, NewName}}
                    },
                    manage_users_helper(Users)
            end
    ;
        % Remove a user from the list.
        {_Caller, disconnection, {User}} ->
            manage_users_helper(Users -- [User])
    end.


is_name_in_use(_Name, []) -> false;
is_name_in_use(Name, [User|OtherUsers]) ->
    case get_name(User) of
        % User is named Name; Name is in use.
        Name ->
            true
    ;
        % User is not named Name; see if anyone else is using that name.
        OtherName when OtherName /= Name ->
            is_name_in_use(Name, OtherUsers)
    end.

% Map

manage_map(Rows, Columns) ->
    Map = create_map(Rows, Columns),
    manage_map_helper(Map).

manage_map_helper(Map) ->
    receive
        {Caller, get_map, {}} ->
            Caller ! {ok, {get_map, {}}, {Map}},
            manage_map_helper(Map)
    end.

create_map(Rows, Columns) ->
    UninitializedMap = array_2d:new({Rows, Columns}),
    FillFunction = fun
        ({R, _}, _) when (R == 0) or (R == Rows-1) ->
            {map_cell, wall}
    ;
        ({_, C}, _) when (C == 0) or (C == Columns-1) ->
            {map_cell, wall}
    ;
        ({_, _}, _) ->
            {map_cell, floor}
    end,
    array_2d:map(FillFunction, UninitializedMap).

get_all_squares_in_map(Map) ->
    % Ask the map for its squares; return their response.
    Map ! {self(), get_map, {}},
    receive
        {ok, {get_map, {}}, {MapSquares}} -> MapSquares
    end.








% Miscellaneous utilities

% strip_whitespace(String, Direction):
%   Removes all whitespace from one or both ends of String.
%   Choices for Direction are: left, right, both.
%
% Explanation: the built-in function string:strip only strips space characters
% (' '), not other whitespace (tabs and newlines).
% I guess this is probably related to the fact that Erlang isn't exactly the
% best language for string processing.
% Credit to
%   http://stackoverflow.com/questions/12794358/how-to-strip-all-blank-characters-in-a-string-in-erlang
% for the regular expressions.
strip_whitespace(String, left) ->
    re:replace(String, "^\\s+", "", [global,{return,list}]);
strip_whitespace(String, right) ->
    re:replace(String, "\\s+$", "", [global,{return,list}]);
strip_whitespace(String, both) ->
    LeftStripped = strip_whitespace(String, left),
    strip_whitespace(LeftStripped, right).

strip_whitespace(String) -> strip_whitespace(String, both).

