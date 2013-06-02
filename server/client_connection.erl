-module(client_connection).

% The number of bytes in a packet header.
% DO NOT CHANGE THIS unless you also change it in the client.
-define(HEADER_SIZE, 2).

-export([
    send/2,
    recv/1,
    recv/2
]).

encode_header(Size) ->
    case encode_header_helper(Size, ?HEADER_SIZE) of
        {ok, Binary} ->
            Binary
    ;
        SomethingElse ->
            SomethingElse
    end.

encode_header_helper(0, 0) ->
    {ok, <<>>};
encode_header_helper(_, 0) ->
    {error, packet_too_big};
encode_header_helper(Size, BytesLeft) when BytesLeft > 0 ->
    LastByte = Size band 16#ff,
    case encode_header_helper(Size bsr 8, BytesLeft - 1) of
        {ok, OtherBytes} ->
            {ok, <<OtherBytes/binary, LastByte>>}
    ;
        {error, TypeOfError} ->
            {error, TypeOfError}
    end.

send(Socket, Binary) ->
    Size = size(Binary),
    ok = gen_tcp:send(Socket, encode_header(Size)),
    gen_tcp:send(Socket, Binary).

% FIXME: The current implementation might be quadratic because every time we
% do another read we copy both the data so far and the new data.
% We can fix this by keeping track of everything as a list and then flattening
% it at the end, but I'm not sure we're ever actually doing more than about 2
% reads, so it may not be worth it.
recv_with_length(Socket, Length, Timeout) ->
    recv_with_length_helper(Socket, Length, Timeout, <<>>).

recv_with_length_helper(Socket, Length, Timeout, Binary) ->
    case size(Binary) of
        Length ->
            {ok, Binary}
    ;
        Size when Size < Length ->
            % The timeout is per call and not in total.
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
    case recv_with_length(Socket, ?HEADER_SIZE, Timeout) of
        {ok, LengthAsBinary} ->
            Length = binary:decode_unsigned(LengthAsBinary),
            recv_with_length(Socket, Length, Timeout)
    ;
        SomethingElse ->
            SomethingElse
    end.

recv(Socket) ->
    recv(Socket, infinity).

