-module(map).

-export([
% For calling
    get_map_cell/2,
    get_map_size/1,
% For spawning
    manage_map/1
]).

-include("map-cell.hrl").


get_map_cell(MapManager, Position) ->
    MapManager ! {self(), request_cell, {Position}},
    receive
        {ok, {request_cell, {Position}}, Cell} ->
            Cell
    end.

get_map_size(MapManager) ->
    MapManager ! {self(), request_size, {}},
    receive
        {ok, {request_size, {}}, Size} ->
            Size
    end.



manage_map(Size) ->
    Map = create_map(Size),
    manage_map_helper(Map).

manage_map_helper(Map) ->
    receive
        {Sender, request_size, {}} ->
            Size = array_2d:size(Map),
            Sender ! {ok, {request_size, {}}, Size},
            manage_map_helper(Map)
    ;
        {Sender, request_cell, {Position}} ->
            Cell = array_2d:get(Position, Map),
            Sender ! {ok, {request_cell, {Position}}, Cell},
            manage_map_helper(Map)
    ;
        {_Sender, new_actor, {Position, ActorInfo}} ->
            % FIXME: Make argument order more consistent.
            NewMap = add_actor_to_map(Map, ActorInfo, Position),
            manage_map_helper(NewMap)
    ;
        {_Sender, move_actor, {ActorInfo, NewPosition}} ->
            OldPosition = user_info_manager:get_actor_position(ActorInfo),
            
            MapWithoutActor = remove_actor_from_map(Map, ActorInfo,
                                                    OldPosition),
            MapWithActorMoved = add_actor_to_map(MapWithoutActor, ActorInfo,
                                                 NewPosition),
            
            % Tell the actor it moved.
            % FIXME: Should we tell the ActorInfo or the ActorController?
            ActorInfo ! {self(), move_in_map, NewPosition},
            
            manage_map_helper(MapWithActorMoved)
    end.

create_map(Size = {Rows, Columns}) ->
    UninitializedMap = array_2d:new(Size),
    FillFunction = fun
        ({R, _}, _) when (R == 0) or (R == Rows-1) ->
            #map_cell{blocks_passage=true}
    ;
        ({_, C}, _) when (C == 0) or (C == Columns-1) ->
            #map_cell{blocks_passage=true}
    ;
        ({_, _}, _) ->
            #map_cell{blocks_passage=false}
    end,
    array_2d:map(FillFunction, UninitializedMap).

remove_actor_from_map(Map, ActorInfo, Position) ->
    OldCell = array_2d:get(Position, Map),
    RemainingActors = OldCell#map_cell.actors -- [ActorInfo],
    NewCell = OldCell#map_cell{actors=RemainingActors},
    array_2d:set(Position, NewCell, Map).

add_actor_to_map(Map, ActorInfo, Position) ->
    OldCell = array_2d:get(Position, Map),
    ActorsInNewCell = [ActorInfo | OldCell#map_cell.actors],
    NewCell = OldCell#map_cell{actors=ActorsInNewCell},
    array_2d:set(Position, NewCell, Map).









