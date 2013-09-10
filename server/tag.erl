-module(tag).

-export([
    tag_allocator_handler/4
]).

% This function should be used as the handler of a process
% (see inter_process:main_loop/2).
% The resulting process is in charge of allocating and deallocating tags.
% 
% The Data tuple given to this process when it's spawned should be an empty
% tuple.
% 
% Having a single tag-managing thread will probably be too much of a bottleneck
% for a distributed server. However, once we reach such a large scale, we
% should be able to divide up tags by map or some such. As long as each tag
% has a prefix that is unique to its map (or server, or whatever), then
% distributed servers can generate unique tags without needing a centralized
% tag manager.
% 
% Because not everything with a tag will have a PID (for example, items have
% tags but not PIDs), we'll eventually have to come up with a way of avoiding
% running out of tags.
% 
% Makes blocking requests of no one.

% Just spawned; set up default values and call the appropriate version of the
% handler (yay, pattern matching!).
tag_allocator_handler({}, MessageType, MessageCommand, MessageArguments) ->
    tag_allocator_handler({10}, MessageType, MessageCommand, MessageArguments);

% Someone wants a new tag.
tag_allocator_handler({Previous}, request, new_tag, {}) ->
    % Give them the next integer.
    NewTag = Previous + 1,
    {handler_continue, {NewTag}, {ok, NewTag}};

% Someone's done with a tag.
tag_allocator_handler({Previous}, notification, free_tag, {_Tag}) ->
    % The OS/161 solution: free is a no-op.
    % FIXME: Don't allow this once we have a server that can be up for
    % prolonged periods of time.
    {handler_continue, {Previous}}.

