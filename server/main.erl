-module(main).

-export([
    run/0
]).

-define(PORT, 6667).

-include("map_cell.hrl").
-include("user_info.hrl").



run() -> wait_for_connections().



wait_for_connections() ->
    TagManager = spawn(tag, manage_tags, []),
    MapManager = spawn(map, manage_map, [{12, 16}]),
    InfoManager = spawn(info_manager, manage_information, [MapManager]),
    
    {ok, ListeningSocket} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
    
    wait_for_connections_helper(ListeningSocket, TagManager, MapManager,
                                InfoManager).

wait_for_connections_helper(ListeningSocket, TagManager, MapManager,
                            InfoManager) ->
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    
    % Create a user for the next connection
    create_user(Socket, TagManager, MapManager, InfoManager),
    
    % Continue waiting for connections
    wait_for_connections_helper(ListeningSocket, TagManager, MapManager,
                                InfoManager).



create_user(Socket, TagManager, MapManager, InfoManager) ->
    Tag = tag:get_new_tag(TagManager),
    
    % FIXME: We seriously need a better way of placing users. But that might
    % have to wait until we have enough maps to actually designate a newbie
    % area....
    Position = {1, 1},
    UserInfo = #user_info{tag=Tag, maps=[MapManager], position=Position},
    
    % TODO: Actually have the user log in.
    
    UserController = spawn(player_control, control_user,
                           [Socket, Tag, InfoManager]),
    UserInfoManager = spawn(user_info_manager, manage_user_info,
                            [UserInfo, UserController]),
    
    InfoManager ! {self(), new_actor, {UserInfoManager, Tag}},
    MapManager ! {self(), new_actor, {Position, UserInfoManager}},
    
    ok.

