% Record for storing information about a single user.
-record(user_info, {
    % tag: the tag (unique identifier) of this user.
    tag,
    
    % username: the name of this player. Currently unused.
    username = "",
    
    % maps: a list of maps containing this player. The only reason there would
    % be more than one map in this list is if the player is in a region in
    % which multiple maps overlap.
    % FIXME: Are we actually overlapping maps?
    maps = [],
    
    % position: the coordinates of the player in their current map.
    % FIXME: If we're allowing players to be in multiple maps, we'll need to
    % do something better than this. And if we're not, map (above) shouldn't be
    % a list. We should make up our minds. And probably make map not a list for
    % now.
    position
}).

