-record(user_info, {
    tag,
    username = "",
    % FIXME: Are we actually overlapping maps?
    maps = [],
    position
}).
