-module(array_2d).

-export([
    new/1,
    get/2,
    set/3,
    size/1,
    map/2
]).

new({Rows, Columns}) ->
    OuterArray = array:new(Rows, fixed),
    array:map(fun(_, _) -> array:new(Columns, fixed) end, OuterArray).

get({Row, Column}, Array) ->
    InnerArray = array:get(Row, Array),
    array:get(Column, InnerArray).

set({Row, Column}, Value, Array) ->
    InnerArray = array:get(Row, Array),
    ModifiedInnerArray = array:set(Column, Value, InnerArray),
    array:set(Row, ModifiedInnerArray, Array).

size(Array) ->
    Rows = array:size(Array),
    Columns = array:size(array:get(0, Array)),
    {Rows, Columns}.

map(Function, Array) ->
    OuterFunction = fun(Row, InnerArray) ->
        array:map(
            fun(Column, Value) ->
                Function({Row, Column}, Value)
            end,
            InnerArray
        )
    end,
    array:map(OuterFunction, Array).
