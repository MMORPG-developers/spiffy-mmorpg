-module(array_2d).

-export([
    new/1,
    get/2,
    set/3,
    size/1,
    map/2
]).

% new(Size)
% Creates a new 2D array of the given size (Rows, Columns).
% Note: behavior is undefined if either Rows or Columns is not a positive
% integer.
new({Rows, Columns}) ->
    % Create an outer array with the right number of rows.
    OuterArray = array:new(Rows, fixed),
    
    % Replace each element of that array with an array with the right number of
    % columns.
    array:map(fun(_, _) -> array:new(Columns, fixed) end, OuterArray).

% get(Index, Array)
% Returns the value in Array at the specified Index (Row, Column).
get({Row, Column}, Array) ->
    % Look up the row, then look up the element.
    InnerArray = array:get(Row, Array),
    array:get(Column, InnerArray).

% set(Index, Value, Array)
% Returns the given array, but with the element at the specified
% Index (Row, Column) replaced by Value.
set({Row, Column}, Value, Array) ->
    % Generate a new inner array.
    InnerArray = array:get(Row, Array),
    ModifiedInnerArray = array:set(Column, Value, InnerArray),
    
    % Put it into the outer array.
    array:set(Row, ModifiedInnerArray, Array).

% size(Array)
% Returns the size (Rows, Columns) of the given array.
size(Array) ->
    Rows = array:size(Array),
    Columns = array:size(array:get(0, Array)),
    {Rows, Columns}.

% map(Function, Array)
% Returns an array where the element at each Index is
% Function(Index, Value), where Value is the element of Array at that Index.
map(Function, Array) ->
    % Use array:map on the outer array to call a function on each inner array.
    % Make that function map a wrapper around the given Function onto the inner
    % array. That wrapper function should translate the index appropriately and
    % then call the given Function.
    
    % The function that will be called on each inner array.
    OuterFunction = fun(Row, InnerArray) ->
        % Use a second call to array:map to get at the individual elements of
        % the inner array.
        array:map(
            % This function is a wrapper around Function; it takes advantage of
            % the fact that it's being defined inside of a fun that knows the
            % Row to give the Row and Column both to the caller's Function.
            fun(Column, Value) ->
                Function({Row, Column}, Value)
            end,
            InnerArray
        )
    end,
    array:map(OuterFunction, Array).

