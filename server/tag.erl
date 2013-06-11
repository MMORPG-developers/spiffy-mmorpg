-module(tag).

-export([
% For calling
    get_new_tag/1,
    free_tag/2,
% For spawning
    manage_tags/0
]).



get_new_tag(TagManager) ->
    TagManager ! {self(), request_tag, {}},
    receive
        {ok, {request_tag, {}}, Tag} ->
            Tag
    end.

free_tag(TagManager, Tag) ->
    TagManager ! {self(), free_tag, {Tag}}.



% Having a single tag-managing thread will probably be too much of a bottleneck
% for a distributed server. However, once we reach such a large scale, we
% should be able to divide up tags by map or some such. As long as each tag
% has a prefix that is unique to its map (or server, or whatever), then
% distributed servers can generate unique tags without needing a centralized
% tag manager.
% 
% As an additional note: running out of tags that the client can handle is
% really not a concern; we should run out of PIDs long before then.

manage_tags() ->
    % Don't generate tags too small just in case it makes a difference.
    manage_tags_helper(10).

manage_tags_helper(Previous) ->
    receive
        {Sender, request_tag, {}} ->
            NewTag = Previous + 1,
            Sender ! {ok, {request_tag, {}}, NewTag},
            manage_tags_helper(NewTag)
    ;
        {_Sender, free_tag, {_Tag}} ->
            % The OS/161 solution: free is a no-op.
            % FIXME: Don't allow this in production code.
            manage_tags_helper(Previous)
    end.

