% FIXME: Maybe rename this module to something more like just 'process'?
-module(inter_process).

% FIXME: Consistency
-export([
% For external use
    spawn_with_handler/2,
    send_notification/3,
    make_request/3,
% For spawning
    main_loop/2
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
% The Handler function should take 4 arguments:
%   * Data --
%       The Data object from above.
%   * MessageType --
%       An atom, either 'request' or 'notification'.
%   * MessageCommand --
%       An atom indicating the command in the message.
%   * MessageArguments --
%       A tuple containing the arguments to the command.
% 
% In the case of a notification, the Handler function should return a 2-tuple
% {Status, NewData}. In the case of a request, it should return a
% 3-tuple {Status, NewData, Response}.
% Status should be an atom, either 'handler_continue' or 'handler_end',
% indicating whether this process should keep executing or stop executing
% (respectively).
% NewData should be the (possibly modified) value of Data to be
% passed to the next iteration of the loop. Response (if applicable) should be
% the Erlang object to be sent back to the requester. It should be a 2-tuple,
% either {ok, Response} or {error, Reason}.
main_loop(Handler = {Module, Function}, Data) ->
    % Get the next message.
    receive
        {request, Sender, MessageCommand, MessageArguments} ->
            % Handle a request.
            {Status, NewData, Response} = apply(Module, Function,
                [Data, request, MessageCommand, MessageArguments]),
            % Send the response back to the requester.
            send_response(Sender, Response, MessageCommand, MessageArguments)
    ;
        {notification, MessageCommand, MessageArguments} ->
            % Handle a notification.
            {Status, NewData} = apply(Module, Function,
                [Data, notification, MessageCommand, MessageArguments])
    ;
        % FIXME: I'd rather avoid encoding the syntax for responses in three
        % places. Ideally it should only show up where it's sent (above) and
        % received (in make_request).
        {response, _MessageCommand, _RequestedData} ->
            % Because we time out when it takes too long to receive a response
            % to a request, receiving unexpected responses is not an error.
            % Instead, simply ignore it and go on with life.
            Status = handler_continue,
            NewData = Data
    ;
        _ ->
            % Crash if we get a malformed message.
            error(invalid_message),
            % Set dummy values for Status and NewData so that those variables
            % are bound in all branches of the receive. (These two lines should
            % never be executed.)
            Status = handler_end,
            NewData = Data
    end,
    case Status of
        handler_continue ->
            % Keep going.
            main_loop(Handler, NewData)
    ;
        handler_end ->
            % End this process.
            ok
    end.


% Spawns a process using the given Handler and Data.
% Handler and Data should be exactly as they would be for main_loop/2
% (see comment on that function); this is just a wrapper around calling
% spawn on main_loop.
spawn_with_handler(Handler, Data) ->
    spawn(?MODULE, main_loop, [Handler, Data]).


% Sends the given notification to the process with the specified Pid.
send_notification(Pid, MessageCommand, MessageArguments) ->
    Pid ! {notification, MessageCommand, MessageArguments}.


% Makes the specified request of the process with the specified Pid.
% MessageCommand should be an atom indicating the nature of the request;
% MessageArguments should be a tuple of arguments clarifying the details of the
% request.
% Returns either {ok, Response} where Response is the response to that request,
% or {error, Reason} if something went wrong.
make_request(Pid, MessageCommand, MessageArguments) ->
    % Send the request.
    Pid ! {request, self(), MessageCommand, MessageArguments},
    receive
        {response, MessageCommand, RequestedData} ->
            % Pass the response on to the caller.
            RequestedData
    after
        ?REQUEST_TIMEOUT ->
            % If no response comes, time out after a while.
            {error, timeout}
    end.



% Private/helper functions



% Send the given Response back to the Sender of a request.
% MessageCommand and MessageArguments are the details of the request (see
% make_request).
% 
% FIXME: Even if we include the entirety of the request in the response,
% there's still a danger that we'll get the wrong response: if a request times
% out and we send the same request, we might see the response to the first
% request and think it's the response to the second request.
% To fix this, add some sort of magic unique identifier to each request.
% For instance, some hash of the current time?
% In theory we could do consecutive integers, but how are we going to keep
% count?
send_response(Sender, Response, MessageCommand, _MessageArguments) ->
    Sender ! {response, MessageCommand, Response}.

