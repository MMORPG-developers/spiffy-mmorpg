-module(user_info_manager).

-export([
% For calling
    get_actor_position/1,
    get_actor_controller/1,
% For spawning
    manage_user_info/2
]).

-include("user_info.hrl").


get_actor_position(UserInfoManager) ->
    UserInfoManager ! {self(), request_coordinates, {}},
    receive
        {ok, {request_coordinates, {}}, Coordinates} ->
            Coordinates
    end.

get_actor_controller(UserInfoManager) ->
    UserInfoManager ! {self(), request_controller, {}},
    receive
        {ok, {request_controller, {}}, Controller} ->
            Controller
    end.


manage_user_info(UserInfo, UserController) ->
    receive
        {Sender, request_controller, {}} ->
            Sender ! {ok, {request_controller, {}}, UserController},
            manage_user_info(UserInfo, UserController)
    ;
        {Sender, request_coordinates, {}} ->
            Position = UserInfo#user_info.position,
            Sender ! {ok, {request_coordinates, {}}, Position},
            manage_user_info(UserInfo, UserController)
    ;
        {_Sender, move_in_map, {NewRow, NewColumn}} ->
            % Calculate the change.
            {OldRow, OldColumn} = UserInfo#user_info.position,
            DeltaRows = NewRow - OldRow,
            DeltaColumns = NewColumn - OldColumn,
            DeltaPosition = {DeltaRows, DeltaColumns},
            
            % Update our position.
            NewUserInfo = UserInfo#user_info{position={NewRow, NewColumn}},
            
            % Pass this on to the UserController.
            % FIXME: Should this in general be the responsibility of the
            % ActorInfo? I'm not convinced that having the controller be
            % notified based on modifications to the underlying data is a good
            % precedent to set.
            UserController ! {no_reply, move_in_map, DeltaPosition},
            
            manage_user_info(NewUserInfo, UserController)
    end.

