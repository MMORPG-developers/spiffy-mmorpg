-module(inter_process).

-export([
% FIXME: Consistency
% For external use
    send_notification/3,
    make_request/3,
    main_loop/2
% For spawning
    % ...
]).

% Amount of time after which to give up on waiting for a request, in
% milliseconds.
-define(REQUEST_TIMEOUT, 5000).



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
%   * MessageCommand --
%       An atom indicating the command in the message.
%   * Arguments --
%       A tuple containing the arguments to the command.
% 
% In the case of a notification, the Handler function should return a 2-tuple
% {Stop, NewData}. In the case of a request, it should return a 3-tuple {Stop,
% NewData, Response}.
% Stop should be a boolean indicating whether this process should stop
% executing. NewData should be the (possibly modified) value of Data to be
% passed to the next iteration of the loop. Response (if applicable) should be
% an arbitrary Erlang object to be sent back to the requester.
% 
% FIXME: What if there's an error? Should the third return be {ok, Response}?
% Or we could make it a 4-tuple, {Stop, NewData, OkOrError, Response}.
main_loop(Handler = {Module, Function}, Data) ->
    % Get the next message.
    receive
        {request, Sender, MessageCommand, Arguments} ->
            % Handle a request.
            {Stop, NewData, Response} = apply(Module, Function,
                [Data, request, MessageCommand, Arguments]),
            % Send the response back to the requester.
            send_response(Sender, Response, MessageCommand, Arguments)
    ;
        {notification, MessageCommand, Arguments} ->
            % Handle a notification.
            {Stop, NewData} = apply(Module, Function,
                [Data, notification, MessageCommand, Arguments])
    ;
        _ ->
            % Crash if we get a malformed message.
            error(invalid_message),
            % Set dummy values for Stop and NewData so that those variables are
            % bound in all branches of the receive. (These two lines should
            % never be executed.)
            Stop = true,
            NewData = Data
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


% Sends the given notification to the process with the specified Pid.
send_notification(Pid, MessageCommand, Arguments) ->
    Pid ! {notification, MessageCommand, Arguments}.


% Makes the specified request of the process with the specified Pid.
% MessageCommand should be an atom indicating the nature of the request;
% Arguments should be a tuple of arguments clarifying the details of the
% request.
% Returns either {ok, Response} where Response is the response to that request,
% or {error, Reason} if something went wrong.
make_request(Pid, MessageCommand, Arguments) ->
    % Send the request.
    Pid ! {request, self(), MessageCommand, Arguments},
    % FIXME: Add a single unifying atom to the beginning? Say, 'response'?
    receive
        {ok, MessageCommand, Response} ->
            % Successfully got a response.
            {ok, Response}
    ;
        {error, Reason} ->
            % Recipient sent back an error.
            {error, Reason}
    after
        ?REQUEST_TIMEOUT ->
            {error, timeout}
    end.

% Send the given Response back to the Sender of a request.
% MessageCommand and Arguments are the details of the request (see
% make_request).
send_response(Sender, Response, MessageCommand, _Arguments) ->
    Sender ! {ok, MessageCommand, Response}.

