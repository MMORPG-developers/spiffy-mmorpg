-module(player_info_manager).

-export([
    handler/4
]).

-include("player_info.hrl").

% This function should be used as the handler of a process
% (see inter_process:main_loop/2).
% The resulting process is in charge of storing the information about a
% particular player.
% 
% The initial arguments to this process should be two values:
% {PlayerInfo, PlayerController}.
% PlayerInfo is the player_info record containing the information about this
% player.
% PlayerController is the PID of the process controlling the player.
% 
% For subsequent iterations, the Data tuple will contain those same two values.

% Just spawned.
handler({}, setup, _, {PlayerInfo, PlayerController}) ->
    {handler_continue, {PlayerInfo, PlayerController}};

% Someone's requested the PID of the controlling process.
handler(Data = {_PlayerInfo, PlayerController},
        request, get_controller, {}) ->
    % Send it back to them.
    {handler_continue, Data, {ok, PlayerController}};

% Someone's requested this player's position in its map.
handler(Data = {PlayerInfo, _PlayerController},
        request, get_position, {}) ->
    % Get it from the player_info record and send it back to them.
    Position = PlayerInfo#player_info.position,
    {handler_continue, Data, {ok, Position}};

% Someone's requested this player's origin coordinates.
handler(Data = {PlayerInfo, _PlayerController},
        request, get_origin, {}) ->
    % Get it from the player_info record and send it back to them.
    Origin = PlayerInfo#player_info.origin,
    {handler_continue, Data, {ok, Origin}};

% Someone's requested this player's origin coordinates.
handler({PlayerInfo, PlayerController},
        notification, move_in_map, {NewRow, NewColumn}) ->
    % Update our position.
    NewPlayerInfo = PlayerInfo#player_info{position={NewRow, NewColumn}},
    {handler_continue, {NewPlayerInfo, PlayerController}}.

