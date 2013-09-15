% XXX: Maybe rename this module to something more like just 'process'?
% Except don't actually use 'process'; vim thinks it's a special word.
-module(inter_process).

-export([
% For external use
    spawn_with_handler/2,
    notify/3,
    make_request/3,
    notify_all/3,
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
%       An atom, either 'request', 'notification', or 'setup'.
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
% 
% Before the main loop begins, the handler will be called once to set up the
% Data tuple. In this call, MessageType will be the atom 'setup',
% MessageArguments will be a tuple of initial arguments passed by whoever
% spawned this process, Data will be an empty tuple, and MessageCommand will
% be arbitrary. Thus every handler should have a clause that matches
%     handler({}, setup, _, InitialArguments).
% In this case, the return format is {Status, Data}, as with a notification.
% It is permitted to return handler_end as Status; this will cause the process
% to terminate without ever handling a message.
main_loop(Handler = {Module, Function}, InitialArguments) ->
    {Status, Data} = apply(Module, Function,
        [{}, setup, setup, InitialArguments]),
    case Status of
        handler_continue ->
            % Enter main loop.
            main_loop_helper(Handler, Data)
    ;
        handler_end ->
            % End this process before it even gets started.
            ok
    end.

main_loop_helper(Handler = {Module, Function}, Data) ->
    % Get the next message.
    receive
        {request, Sender, RequestId, MessageCommand, MessageArguments} ->
            % Handle a request.
            {Status, NewData, Response} = apply(Module, Function,
                [Data, request, MessageCommand, MessageArguments]),
            % Send the response back to the requester.
            send_response(Sender, Response, RequestId)
    ;
        {notification, MessageCommand, MessageArguments} ->
            % Handle a notification.
            {Status, NewData} = apply(Module, Function,
                [Data, notification, MessageCommand, MessageArguments])
    ;
        % FIXME: I'd rather avoid encoding the syntax for responses in three
        % places. Ideally it should only show up where it's sent (above) and
        % received (in make_request).
        {response, _MessageId, _RequestedData} ->
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
            main_loop_helper(Handler, NewData)
    ;
        handler_end ->
            % End this process.
            ok
    end.


% Spawns a process using the given Handler and Data.
% Handler and InitialArguments should be exactly as they would be for
% main_loop/2 (see comment on that function); this is just a wrapper around
% calling spawn on main_loop.
spawn_with_handler(Handler, InitialArguments) ->
    spawn(?MODULE, main_loop, [Handler, InitialArguments]).


% Sends the given notification to the process with the specified Pid.
notify(Pid, MessageCommand, MessageArguments) ->
    Pid ! {notification, MessageCommand, MessageArguments}.


% notify_all(Processes, MessageCommand, MessageArguments):
% Sends the given notification to all of the given processes.
notify_all([], _MessageCommand, _MessageArguments) ->
    ok;
notify_all([Pid|OtherProcesses], MessageCommand, MessageArguments) ->
    notify(Pid, MessageCommand, MessageArguments),
    notify_all(OtherProcesses, MessageCommand, MessageArguments).


% Makes the specified request of the process with the specified Pid.
% MessageCommand should be an atom indicating the nature of the request;
% MessageArguments should be a tuple of arguments clarifying the details of the
% request.
% Returns either {ok, Response} where Response is the response to that request,
% or {error, Reason} if something went wrong.
make_request(Pid, MessageCommand, MessageArguments) ->
    % Get an identifier so we can match the response to the request.
    RequestId = get_request_identifier(),
    
    % Send the request.
    Pid ! {request, self(), RequestId, MessageCommand, MessageArguments},
    receive
        {response, RequestId, RequestedData} ->
            % Pass the response on to the caller.
            RequestedData
    after
        ?REQUEST_TIMEOUT ->
            % If no response comes, time out after a while.
            {error, timeout}
    end.



% Private/helper functions



% Send the given Response back to the Sender of a request.
% MessageId is the magic identifier sent with the original message, as computed
% by get_request_identifier/0.
send_response(Sender, Response, MessageId) ->
    Sender ! {response, MessageId, Response}.

% Return a unique identifier to be used with the next request so that its
% response can be uniquely matched to it.
get_request_identifier() ->
    {_Megaseconds, Seconds, Microseconds} = now(),
    Seconds bxor Microseconds.

