-module(main).

-export([
    start/0
]).

-define(PORT, 6667).

-include("map_cell.hrl").
-include("player_info.hrl").



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
wait_for_connections() ->
    % Spawn all the infrastructure we need.
    % FIXME: This process farm is getting out of hand. There should probably be
    % a separate function that initializes all this. But I'm putting that off
    % until we have a sense of just how many PIDs we'll need to pass around --
    % much more than what we've got now and we'll need a better solution than
    % just having every function take all the PIDs as arguments.
    TagAllocator = inter_process:spawn_with_handler(
        {tag_allocator, handler}, {}),
    TagAssignments = inter_process:spawn_with_handler(
        {tag_assignment_manager, handler}, {}),
    MapManager = inter_process:spawn_with_handler(
        {map_manager, handler}, {{12, 16}}),
    InfoManager = inter_process:spawn_with_handler(
        {info_manager, handler}, {MapManager, TagAssignments}),
    CommandExecutor = inter_process:spawn_with_handler(
        {command_executor, handler}, {MapManager, TagAssignments}),
    
    % The MapManager and InfoManager both need to send messages to each other.
    % Since one of them must be created first, we create the circular reference
    % by sending a message to the first one created with the PID of the second
    % one.
    % FIXME: Is this actually a clean way of doing things? As long as the
    % MapManager never makes blocking requests of the InfoManager we should be
    % safe from deadlock, but creating circular references like this seems like
    % questionably good design practice. That said, I don't see a better way of
    % doing this.
    inter_process:send_notification(MapManager, subscribe, {InfoManager}),
    
    % Create a socket to listen for connections.
    {ok, ListeningSocket} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
    
    wait_for_connections_helper(ListeningSocket, TagAllocator, MapManager,
                                InfoManager, CommandExecutor).

wait_for_connections_helper(ListeningSocket, TagAllocator, MapManager,
                            InfoManager, CommandExecutor) ->
    % Accept a new connection.
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    
    % Create a new player for the connection.
    create_player(Socket, TagAllocator, MapManager, InfoManager,
        CommandExecutor),
    
    % Wait for more connections.
    wait_for_connections_helper(ListeningSocket, TagAllocator, MapManager,
                                InfoManager, CommandExecutor).



% create_player(Socket, TagAllocator, MapManager, InfoManager,
%               CommandExecutor).
% Does all the necessary setup for a new player.
% TagAllocator is the PID of the process that allocates tags.
% MapManager is the PID of the process that manages the map.
% InfoManager is the PID of the process that manages information distribution.
create_player(Socket, TagAllocator, MapManager, InfoManager,
              CommandExecutor) ->
    % Get a tag for the new player.
    {ok, Tag} = inter_process:make_request(TagAllocator, new_tag, {}),
    
    % Create an info record for the player.
    % For now, just put them in the top-left corner.
    % FIXME: We seriously need a better way of placing players. But that might
    % have to wait until we have enough maps to actually designate a newbie
    % area....
    % FIXME: Actually have the player log in and assign them a preexisting
    % character, rather than simply creating a fresh one for every login.
    Position = {1, 1},
    PlayerInfo = #player_info{tag=Tag, maps=[MapManager], position=Position,
                          origin=Position},
    
    % Spawn two processes for the player: one to control it and the other to
    % store its information.
    PlayerController = inter_process:spawn_with_handler(
        {player_control, handler},
        {Socket, Tag, InfoManager, CommandExecutor}),
    PlayerInfoManager = inter_process:spawn_with_handler(
        {player_info_manager, handler}, {PlayerInfo, PlayerController}),
    
    % Tell the info manager and map manager someone's joined the server.
    inter_process:send_notification(
        InfoManager, new_actor, {PlayerInfoManager, Tag}),
    inter_process:send_notification(
        MapManager, new_actor, {Position, PlayerInfoManager}),
    
    ok.

