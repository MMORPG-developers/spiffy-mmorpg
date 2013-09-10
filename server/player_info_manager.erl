-module(player_info_manager).

-export([
    handler/4
]).

-include("handler.hrl").
-include("user_info.hrl").

% This function should be used as the handler of a process
% (see inter_process:main_loop/2).
% The resulting process is in charge of storing the information about a
% particular player.
% 
% The Data tuple given to this process when it's spawned should contain
% two values: {UserInfo, UserController}.
% UserInfo is the user_info record containing the information about this
% player.
% UserController is the PID of the process controlling the player.
% 
% Makes blocking requests of no one.

% Someone's requested the PID of the controlling process.
handler(Data = {_UserInfo, UserController},
        request, get_controller, {}) ->
    % Send it back to them.
    {?HANDLER_CONTINUE, Data, {ok, UserController}};

% Someone's requested this user's position in its map.
handler(Data = {UserInfo, _UserController},
        request, get_position, {}) ->
    % Get it from the user_info record and send it back to them.
    Position = UserInfo#user_info.position,
    {?HANDLER_CONTINUE, Data, {ok, Position}};

% Someone's requested this user's origin coordinates.
handler(Data = {UserInfo, _UserController},
        request, get_origin, {}) ->
    % Get it from the user_info record and send it back to them.
    Origin = UserInfo#user_info.origin,
    {?HANDLER_CONTINUE, Data, {ok, Origin}};

% Someone's requested this user's origin coordinates.
handler({UserInfo, UserController},
        notification, move_in_map, {NewRow, NewColumn}) ->
    % Update our position.
    NewUserInfo = UserInfo#user_info{position={NewRow, NewColumn}},
    {?HANDLER_CONTINUE, {NewUserInfo, UserController}}.

