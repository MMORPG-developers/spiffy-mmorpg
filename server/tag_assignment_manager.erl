-module(tag_assignment_manager).

-export([
    handler/4
]).



% This function should be used as the handler of a process
% (see inter_process:main_loop/2).
% The resulting process serves as the authority on who has what tag. If anyone
% wants to look up a tag, this is the process to go to.
% 
% The initial arguments to this process should be an empty tuple.
% 
% For subsequent iterations, the Data tuple will contain one value:
% a dictionary mapping tags to actor info PIDs.

% Just spawned.
handler({}, setup, _, {}) ->
    % Initialize the tag dictionary.
    TagDict = dict:new(),
    
    {handler_continue, {TagDict}};

% Create a new (Tag, ActorInfo) mapping.
% ActorInfo should be the PID of the process storing the actor's
% information, and Tag should be the numerical tag of the new actor.
handler({TagDict}, notification, assign_tag,
        {Tag, ActorInfo}) ->
    % Use a case block so that we crash if the Tag is already in use.
    % FIXME: Crash better if the tag is already in use.
    case dict:is_key(Tag, TagDict) of
        false ->
            % Map Tag to ActorInfo in the dictionary.
            NewTagDict = dict:append(Tag, ActorInfo, TagDict),
            {handler_continue, {NewTagDict}}
    end;

% Remove the mapping from the specified Tag to its actor.
handler({TagDict}, notification, unassign_tag,
        {Tag}) ->
    % Use a case block so that we crash if the Tag is not in use.
    % FIXME: Crash better if the tag is not in use.
    case dict:is_key(Tag, TagDict) of
        true ->
            % Remove the (Tag, ActorInfo) mapping from the dictionary.
            NewTagDict = dict:erase(Tag, TagDict),
            {handler_continue, {NewTagDict}}
    end;

% Look up the specified Tag, returning the actor to which it is mapped.
handler({TagDict}, request, lookup_tag,
        {Tag}) ->
    case dict:is_key(Tag, TagDict) of
        true ->
            [ActorInfo] = dict:fetch(Tag, TagDict),
            {handler_continue, {TagDict}, {ok, ActorInfo}}
    ;
        false ->
            {handler_continue, {TagDict}, {error, invalid_tag}}
    end.

