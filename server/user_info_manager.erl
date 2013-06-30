-module(user_info_manager).

-export([
% For calling
    get_actor_position/1,
    get_actor_origin/1,
    get_actor_controller/1,
% For spawning
    manage_user_info/2
]).

-include("user_info.hrl").


% get_actor_position(UserInfoManager)
% Returns the coordinates of the given actor in its map.
% UserInfoManager is the PID of the process storing the actor's information.
% FIXME: This function isn't even well-defined if we overlap maps....
get_actor_position(UserInfoManager) ->
    % Trust the UserInfoManager to have the correct coordinates.
    % Just ask it where it is...
    UserInfoManager ! {self(), request_coordinates, {}},
    receive
        % ...and return its response.
        {ok, {request_coordinates, {}}, Coordinates} ->
            Coordinates
    end.

% get_actor_origin(UserInfoManager)
% Returns the "origin coordinates" of the given actor. Any coordinates
% communicated to this actor should be given relative to the origin
% coordinates.
% UserInfoManager is the PID of the process storing the actor's information.
get_actor_origin(UserInfoManager) ->
    % Request the coordinates.
    UserInfoManager ! {self(), request_origin, {}},
    receive
        % Wait for the response; return it.
        {ok, {request_origin, {}}, Origin} ->
            Origin
    end.

% get_actor_controller(UserInfoManager)
% Returns the PID of the process controlling the specified player.
% UserInfoManager is the PID of the process storing its information.
get_actor_controller(UserInfoManager) ->
    % Request the controlling PID.
    UserInfoManager ! {self(), request_controller, {}},
    receive
        % Wait for the response; return it.
        {ok, {request_controller, {}}, Controller} ->
            Controller
    end.


% manage_user_info(UserInfo, UserController)
% This function should be spawned as a process.
% The resulting process is in charge of storing the information about a
% particular player.
% UserInfo is the user_info record containing the information about this
% player.
% UserController is the PID of the process controlling the player.
% 
% Makes blocking requests of no one.
manage_user_info(UserInfo, UserController) ->
    receive
        % Someone's requested the PID of the controlling process.
        {Sender, request_controller, {}} ->
            % Send it back to them.
            Sender ! {ok, {request_controller, {}}, UserController},
            manage_user_info(UserInfo, UserController)
    ;
        % Someone's requested this user's position in its map.
        {Sender, request_coordinates, {}} ->
            % Get it from the user_info record and send it back to them.
            Position = UserInfo#user_info.position,
            Sender ! {ok, {request_coordinates, {}}, Position},
            manage_user_info(UserInfo, UserController)
    ;
        % Someone's requested this user's origin coordinates.
        {Sender, request_origin, {}} ->
            % Get it from the user_info record and send it back to them.
            Origin = UserInfo#user_info.origin,
            Sender ! {ok, {request_origin, {}}, Origin},
            manage_user_info(UserInfo, UserController)
    ;
        % Someone's told us to move to a new location.
        % Note: telling the user info process will NOT cause the map to be
        % updated.
        {_Sender, move_in_map, {NewRow, NewColumn}} ->
            % Calculate the change in position.
            {OldRow, OldColumn} = UserInfo#user_info.position,
            DeltaRows = NewRow - OldRow,
            DeltaColumns = NewColumn - OldColumn,
            DeltaPosition = {DeltaRows, DeltaColumns},
            
            % Update our position.
            NewUserInfo = UserInfo#user_info{position={NewRow, NewColumn}},
            
            % Inform the UserController.
            % FIXME: Should this in general be the responsibility of the
            % ActorInfo? I'm not convinced that having the controller be
            % notified whenever the underlying data is modified is a good
            % precedent to set -- you can imagine something changing about a
            % player that they don't know about.
            UserController ! {no_reply, move_in_map, DeltaPosition},
            
            manage_user_info(NewUserInfo, UserController)
    end.

