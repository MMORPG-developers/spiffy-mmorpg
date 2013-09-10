-module(main).

-export([
% For calling
    start/0
% For spawning
    % wait_for_connections/0
]).

-define(PORT, 6667).

-include("map_cell.hrl").
-include("user_info.hrl").



% start()
% Start the server.
start() ->
    % For now, just call the function that does the actual work.
    % 
    % TODO: Theoretically, we can spawn that function instead of calling it.
    % When the Erlang shell runs with input redirected from a file, it appears
    % to die as soon as the file runs out of input and the main process stops,
    % so if run() spawns wait_for_connections() then the server immediately
    % quits. But if you run an Erlang shell and manually spawn
    % wait_for_connections(), it runs in the background and you can run other
    % commands (for debugging, for example).
    wait_for_connections().
    % spawn(?MODULE, wait_for_connections, []).



% wait_for_connections()
% Sits around in a loop, accepting new connections.
% 
% Calls create_user, which makes blocking requests of the tag manager.
wait_for_connections() ->
    % Spawn all the infrastructure we need.
    % FIXME: There should probably be a separate function that initializes all
    % this. But I'm putting that off until we have a sense of just how many
    % PIDs we'll need to pass around -- much more than what we've got now and
    % we'll need a better solution than just having every function take all the
    % PIDs as arguments.
    TagAllocator = spawn(inter_process, main_loop,
                         [{tag, tag_allocator_handler}, {}]),
    MapManager = spawn(map, manage_map, [{12, 16}]),
    InfoManager = spawn(info_manager, manage_information, [MapManager]),
    
    % The MapManager and InfoManager both need to send messages to each other.
    % Since one of them must be created first, we create the circular reference
    % by sending a message to the first one created with the PID of the second
    % one.
    % FIXME: Is this actually a clean way of doing things? As long as the
    % MapManager never makes blocking requests of the InfoManager we should be
    % safe from deadlock, but creating circular references like this seems like
    % questionably good design practice. That said, I don't see a better way of
    % doing this.
    MapManager ! {self(), set_info_manager, InfoManager},
    
    % Create a socket to listen for connections.
    {ok, ListeningSocket} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
    
    wait_for_connections_helper(ListeningSocket, TagAllocator, MapManager,
                                InfoManager).

wait_for_connections_helper(ListeningSocket, TagAllocator, MapManager,
                            InfoManager) ->
    % Accept a new connection.
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    
    % Create a new user for the connection.
    create_user(Socket, TagAllocator, MapManager, InfoManager),
    
    % Wait for more connections.
    wait_for_connections_helper(ListeningSocket, TagAllocator, MapManager,
                                InfoManager).



% create_user(Socket, TagAllocator, MapManager, InfoManager).
% Does all the necessary setup for a new user.
% TagAllocator is the PID of the process that allocates tags.
% MapManager is the PID of the process that manages the map.
% InfoManager is the PID of the process that manages information distribution.
% 
% Makes blocking requests of the tag manager.
create_user(Socket, TagAllocator, MapManager, InfoManager) ->
    % Get a tag for the new user.
    Tag = inter_process:make_request(TagAllocator, new_tag, {}),
    
    % Create an info record for the user.
    % For now, just put them in the top-left corner.
    % FIXME: We seriously need a better way of placing users. But that might
    % have to wait until we have enough maps to actually designate a newbie
    % area....
    % FIXME: Actually have the user log in and assign them a preexisting
    % character, rather than simply creating a fresh one for every login.
    Position = {1, 1},
    UserInfo = #user_info{tag=Tag, maps=[MapManager], position=Position,
                          origin=Position},
    
    % Spawn two processes for the user: one to control it and the other to
    % store its information.
    UserController = spawn(player_control, control_user,
                           [Socket, Tag, InfoManager]),
    UserInfoManager = spawn(inter_process, main_loop,
                            [{player_info_manager, handler},
                             {UserInfo, UserController}]),
    
    % Tell the info manager and map manager someone's joined the server.
    InfoManager ! {self(), new_actor, {UserInfoManager, Tag}},
    MapManager ! {self(), new_actor, {Position, UserInfoManager}},
    
    ok.

