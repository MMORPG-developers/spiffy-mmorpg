-module(chatserver).

% TODO: Look into gen_tcp:shutdown.

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
    {ok, ListeningSocket} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
    listen_for_connections_helper(ListeningSocket, UserManager, MessageSender).

listen_for_connections_helper(ListeningSocket, UserManager, MessageSender) ->
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    % TODO: Check for error?
    spawn(?MODULE, handshake_with_user, [Socket, UserManager, MessageSender]),
    listen_for_connections_helper(ListeningSocket, UserManager, MessageSender).


% Handshaker

handshake_with_user(Socket, UserManager, MessageSender) ->
    User = spawn(?MODULE, maintain_user_state, []),
    % Assume this succeeds
    UserManager ! {self(), new_user, {User}},
    MessageSender ! {connection, {{User, Socket}}},
    get_name_for_user(User, Socket, UserManager, MessageSender).

get_name_for_user(User, Socket, UserManager, MessageSender) ->
    ok = gen_tcp:send(Socket, "What is your name?\n"),
    {ok, Data} = gen_tcp:recv(Socket, 0),
    Name = strip_whitespace(binary_to_list(Data)),
    UserManager ! {self(), change_name, {User, Name}},
    receive
        {ok, {change_name, {User, Name}}} ->
            ok = gen_tcp:send(Socket, "Welcome, " ++ Name ++ "!\n"),
            receive_messages(User, Socket, UserManager, MessageSender)
    ;
        {error, name_in_use, {change_name, {User, Name}}} ->
            ok = gen_tcp:send(Socket, "That name is already in use.\n"),
            get_name_for_user(User, Socket, UserManager, MessageSender)
    end.


% State holder

maintain_user_state() -> maintain_user_state_helper({user, ""}).

maintain_user_state_helper({user, Name}) ->
    receive
        {Caller, get_name, {}} ->
            Caller ! {ok, {get_name, {}}, {Name}},
            maintain_user_state_helper({user, Name})
    ;
        {_Caller, change_name, {NewName}} ->
            maintain_user_state_helper({user, NewName})
    end.

get_name(User) ->
    User ! {self(), get_name, {}},
    receive
        {ok, {get_name, {}}, {Name}} -> Name
    end.


% Listener

receive_messages(User, Socket, UserManager, MessageSender) ->
    ok = receive_messages_helper(User, Socket, MessageSender),
    ok = gen_tcp:close(Socket),
    MessageSender ! {disconnection, {{User, Socket}}},
    UserManager ! {self(), disconnection, {User}}.

receive_messages_helper(User, Socket, MessageSender) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Message = binary_to_list(Data),
            MessageSender ! {chat, {{User, Socket}, Message}},
            receive_messages_helper(User, Socket, MessageSender)
    ;
        {error, closed} ->
            ok
    end.


% User manager

manage_users() -> manage_users_helper([]).

manage_users_helper(Users) ->
    receive
        {_Caller, new_user, {User}} ->
            manage_users_helper([User|Users])
    ;
        {Caller, change_name, {User, NewName}} ->
            case is_name_in_use(NewName, Users) of
                false ->
                    User ! {self(), change_name, {NewName}},
                    Caller ! {ok,
                        {change_name, {User, NewName}}
                    },
                    manage_users_helper(Users)
            ;
                true ->
                    Caller ! {error, name_in_use,
                        {change_name, {User, NewName}}
                    },
                    manage_users_helper(Users)
            end
    ;
        {_Caller, disconnection, {User}} ->
            manage_users_helper(Users -- [User])
    end.

is_name_in_use(_Name, []) -> false;
is_name_in_use(Name, [User|OtherUsers]) ->
    case get_name(User) of
        Name ->
            true
    ;
        OtherName when OtherName /= Name ->
            is_name_in_use(Name, OtherUsers)
    end.


% Sender thread

send_messages() -> send_messages_helper([]).

send_messages_helper(Connections) ->
    receive
        {connection, {NewConnection}} ->
            send_messages_helper([NewConnection|Connections])
    ;
        {disconnection, {Disconnected}} ->
            send_messages_helper(Connections -- [Disconnected])
    ;
        {chat, {SendingConnection = {Sender, _SenderSocket}, Message}} ->
            SenderName = get_name(Sender),
            % We don't add a newline here because currently the caller doesn't
            % remove the newline from the data returned by gen_tcp:recv.
            AnnotatedMessage = SenderName ++ ": " ++ Message,
            ok = broadcast(Connections -- [SendingConnection],
                           AnnotatedMessage),
            send_messages_helper(Connections)
    % TODO: When do we quit?
    end.

broadcast([], _Message) -> ok;
broadcast([{_User, Socket} | OtherConnections], Message) ->
    ok = gen_tcp:send(Socket, Message),
    broadcast(OtherConnections, Message).


% Miscellaneous utilities

% The built-in function string:strip only strips space characters (' '),
% not other whitespace (tabs and newlines).
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

