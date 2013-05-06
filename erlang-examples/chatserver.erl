-module(chatserver).

% TODO: Look into gen_tcp:shutdown.
% TODO: When do we quit?
% TODO: Check for errors on spawns and message sendings?
% TODO: Links might be nice

-export([
    % Functions actually used by outside code
        main/0,
    % Functions that we spawn
        manage_users/0,
        send_messages/0,
        handshake_with_user/3,
        maintain_user_state/0
    ]).

-define(PORT, 6667).


%
% User struct: {user, Name}
%



% Main (driver) thread

main() -> listen_for_connections().


listen_for_connections() ->
    UserManager = spawn(?MODULE, manage_users, []),
    MessageSender = spawn(?MODULE, send_messages, []),
    
    % Open a listening socket and wait for connections
    {ok, ListeningSocket} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
    listen_for_connections_helper(ListeningSocket, UserManager, MessageSender).


listen_for_connections_helper(ListeningSocket, UserManager, MessageSender) ->
    % Create a user for the next connection
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    spawn(?MODULE, handshake_with_user, [Socket, UserManager, MessageSender]),
    
    % Continue waiting for connections
    listen_for_connections_helper(ListeningSocket, UserManager, MessageSender).



% Handshaker

handshake_with_user(Socket, UserManager, MessageSender) ->
    % Create a state-holding process for the user with a dummy name.
    User = spawn(?MODULE, maintain_user_state, []),
    
    UserManager ! {self(), new_user, {User}},
    MessageSender ! {connection, {{User, Socket}}},
    
    ask_user_for_name(User, Socket, UserManager, MessageSender).


ask_user_for_name(User, Socket, UserManager, MessageSender) ->
    % Get a name from the user
    ok = gen_tcp:send(Socket, "What is your name?\n"),
    {ok, Data} = gen_tcp:recv(Socket, 0),
    Name = strip_whitespace(binary_to_list(Data)),
    
    % Try to change the user's name
    UserManager ! {self(), change_name, {User, Name}},
    receive
        % Success!
        {ok, {change_name, {User, Name}}} ->
            ok = gen_tcp:send(Socket, "Welcome, " ++ Name ++ "!\n"),
            Message = Name ++ " has joined the channel.\n",
            MessageSender ! {special_message, {Message}},
            
            % Start listening for messages from this user.
            receive_messages(User, Socket, UserManager, MessageSender)
    ;
        % The name is taken.
        {error, name_in_use, {change_name, {User, Name}}} ->
            % Inform the user that name is taken; ask them for another one.
            ok = gen_tcp:send(Socket, "That name is already in use.\n"),
            ask_user_for_name(User, Socket, UserManager, MessageSender)
    end.



% State holder

maintain_user_state() -> maintain_user_state_helper({user, ""}).


maintain_user_state_helper({user, Name}) ->
    receive
        % Someone asked us for our name.
        {Caller, get_name, {}} ->
            Caller ! {ok, {get_name, {}}, {Name}},
            maintain_user_state_helper({user, Name})
    ;
        % Someone told us to take a new name.
        {_Caller, change_name, {NewName}} ->
            maintain_user_state_helper({user, NewName})
    end.


get_name(User) ->
    % Ask the user for their name; return their response.
    User ! {self(), get_name, {}},
    receive
        {ok, {get_name, {}}, {Name}} -> Name
    end.



% Listener

receive_messages(User, Socket, UserManager, MessageSender) ->
    % Get messages from the user as long as the connection is open,
    % then close the socket from our end.
    ok = receive_messages_helper(User, Socket, MessageSender),
    ok = gen_tcp:close(Socket),
    
    % Tell the message sender, user manager, and other users that this user
    % disconnected.
    Name = get_name(User),
    MessageSender ! {disconnection, {{User, Socket}}},
    UserManager ! {self(), disconnection, {User}},
    Message = Name ++ " has left the channel.\n",
    MessageSender ! {special_message, {Message}}.


receive_messages_helper(User, Socket, MessageSender) ->
    case gen_tcp:recv(Socket, 0) of
        % Received a chat from the user.
        {ok, Data} ->
            Message = binary_to_list(Data),
            MessageSender ! {chat, {{User, Socket}, Message}},
            receive_messages_helper(User, Socket, MessageSender)
    ;
        % Connection closed.
        {error, closed} ->
            ok
    end.



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



% Sender thread

send_messages() -> send_messages_helper([]).


send_messages_helper(Connections) ->
    receive
        % Add someone else to the list.
        {connection, {NewConnection}} ->
            send_messages_helper([NewConnection|Connections])
    ;
        % Remove someone from the list.
        {disconnection, {Disconnected}} ->
            send_messages_helper(Connections -- [Disconnected])
    ;
        % Send a chat, attaching someone's name to it.
        {chat, {{Sender, _SenderSocket}, Message}} ->
            SenderName = get_name(Sender),
            % We don't add a newline here because currently the caller doesn't
            % remove the newline from the data returned by gen_tcp:recv.
            AnnotatedMessage = SenderName ++ ": " ++ Message,
            ok = broadcast(Connections, AnnotatedMessage),
            send_messages_helper(Connections)
    ;
        % Send a message to everyone, without modifying it.
        {special_message, {Message}} ->
            ok = broadcast(Connections, Message),
            send_messages_helper(Connections)
    end.


% Send a message to everyone.
broadcast([], _Message) -> ok;
broadcast([{_User, Socket} | OtherConnections], Message) ->
    ok = gen_tcp:send(Socket, Message),
    broadcast(OtherConnections, Message).



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

