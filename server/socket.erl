-module(socket).

-export([
    send/2,
    recv/1,
    recv/2
]).

send(Socket, Binary) ->
    Size = size(Binary),
    % Assert Size < 2**32
    SizeAsBinary = binary:encode_unsigned(Size, big),
    ok = gen_tcp:send(Socket, SizeAsBinary),
    gen_tcp:send(Socket, Binary).

% FIXME: This is O(scary), and I'm not sure if it's quick enough in practice.
recv_with_length(Socket, Length, Timeout) ->
    recv_with_length_helper(Socket, Length, Timeout, <<>>).

recv_with_length_helper(Socket, Length, Timeout, Binary) ->
    case size(Binary) of
        Length ->
            {ok, Binary}
    ;
        Size when Size < Length ->
            % FIXME: So the timeout is per call and not in total?
            NextResult = gen_tcp:recv(Socket, Length - Size, Timeout),
            case NextResult of
                {ok, Binary2} ->
                    CombinedBinary = <<Binary/binary, Binary2/binary>>,
                    recv_with_length_helper(Socket, Length, Timeout,
                                            CombinedBinary)
            ;
                SomethingElse ->
                    SomethingElse
            end
    end.

recv(Socket, Timeout) ->
    % FIXME: Magic number (4 -> 32 bits)
    case recv_with_length(Socket, 4, Timeout) of
        {ok, LengthAsBinary} ->
            Length = binary:decode_unsigned(LengthAsBinary),
            recv_with_length(Socket, Length, Timeout)
    ;
        SomethingElse ->
            SomethingElse
    end.

recv(Socket) ->
    recv(Socket, infinity).

