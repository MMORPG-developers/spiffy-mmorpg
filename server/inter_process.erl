-module(inter_process).

-export([
% FIXME: Consistency
% For external use
    main_loop/2
% For spawning
    % wait_for_connections/0
]).



% main_loop(Handler, Data):
% This function should be spawned as a process.
% 
% Handler is a 2-tuple consisting of the name of a module and function as
% atoms. When called, this function goes into a loop, receiving messages from
% other processes and passing them on to the Handler function.
% 
% Data is an arbitrary Erlang object which will be passed to the Handler
% function each time it is called, along with information about the message
% being handled.
% 
% The Handler function should take 5 arguments:
%   * Data --
%       The Data object from above.
%   * MessageType --
%       An atom, either 'request' or 'notification'.
%   * Sender --
%       In the case of a request, the PID of the sender. In the case of a
%       notification, the atom 'no_reply'.
%   * MessageCommand --
%       An atom indicating the command in the message.
%   * Arguments --
%       A tuple containing the arguments to the command.
% 
% The Handler function should return a 2-tuple {Stop, NewData}.
% Stop should be a boolean indicating whether this process should stop
% executing. NewData should be the (possibly modified) value of Data to be
% passed to the next iteration of the loop.
main_loop(Handler = {Module, Function}, Data) ->
    % Get the next message.
    receive
        {request, Sender, MessageCommand, Arguments} ->
            % Handle a request.
            {Stop, NewData} = apply(Module, Function,
                [Data, request, Sender, MessageCommand, Arguments])
            % FIXME: Don't pass the sender to the Handler; instead return a
            % 3-tuple in the case of requests and then send the response
            % message to the caller here.
    ;
        {notification, MessageCommand, Arguments} ->
            % Handle a notification.
            {Stop, NewData} = apply(Module, Function,
                [Data, notification, no_reply, MessageCommand, Arguments])
    ;
        _ ->
            % Crash if we get a malformed message.
            error(invalid_message)
    end,
    case Stop of
        false ->
            % Keep going.
            main_loop(Handler, NewData)
    ;
        true ->
            % Stop.
            ok
    end.

