-module(client_connection).

% The number of bytes in a packet header.
% DO NOT CHANGE THIS unless you also change it in the client.
-define(HEADER_SIZE, 2).

-export([
    send/2,
    recv/1,
    recv/2
]).


% send(Socket, Binary)
% Encodes the given Binary as a packet and sends it on the given Socket.
send(Socket, Binary) ->
    Size = size(Binary),
    ok = gen_tcp:send(Socket, encode_header(Size)),
    gen_tcp:send(Socket, Binary).

% recv(Socket)
% Reads a single packet from the given Socket. Blocks indefinitely, if
% necessary.
recv(Socket) ->
    recv(Socket, infinity).

% recv(Socket, Timeout)
% Reads a single packet from the given Socket.
% Blocks for up to Timeout on each call to the underlying function to read from
% the socket. Note, however, that receiving a single packet on a socket might
% require several such calls, so the total time taken by this function could
% be substantially more than Timeout.
% For details on the Timeout argument, see the documentation for
% gen_tcp:recv/3 at
%   http://www.erlang.org/doc/man/gen_tcp.html#recv-3
recv(Socket, Timeout) ->
    % Read the header.
    case recv_with_length(Socket, ?HEADER_SIZE, Timeout) of
        {ok, LengthAsBinary} ->
            % Decode it, giving the size of the packet. Read the packet.
            Length = binary:decode_unsigned(LengthAsBinary),
            recv_with_length(Socket, Length, Timeout)
    ;
        SomethingElse ->
            % Propagate any errors.
            SomethingElse
    end.


% encode_header(Size)
% Encodes the given packet Size in a packet header. Every packet sent should be
% preceeded by a header as returned by this function.
encode_header(Size) ->
    % Delegate the work to a helper function.
    case encode_header_helper(Size, ?HEADER_SIZE) of
        {ok, Binary} ->
            Binary
    ;
        SomethingElse ->
            SomethingElse
    end.

% encode_header_helper(Size, BytesLeft)
% Encodes the given packet Size in a header of at most BytesLeft bytes.
encode_header_helper(0, 0) ->
    % Base case: zero bytes of nothing.
    {ok, <<>>};
encode_header_helper(_, 0) ->
    % Other base case: Nonzero size but no bytes left in header to encode it.
    {error, packet_too_big};
encode_header_helper(Size, BytesLeft) when BytesLeft > 0 ->
    % Separate the least significant byte.
    LastByte = Size band 16#ff,
    
    % Encode the rest in all but one of the bytes.
    case encode_header_helper(Size bsr 8, BytesLeft - 1) of
        {ok, OtherBytes} ->
            % Concatenate the two parts.
            {ok, <<OtherBytes/binary, LastByte>>}
    ;
        {error, TypeOfError} ->
            % Propagate any errors encountered.
            {error, TypeOfError}
    end.

% recv_with_length(Socket, Length, Timeout)
% Receive exactly Length bytes from the given Socket, returning some sort of
% error if Timeout elapses without any new data coming through.
% 
% FIXME: The current implementation might be quadratic because every time we
% do another read we copy both the data so far and the new data.
% We can fix this by keeping track of everything as a list and then flattening
% it at the end, but I'm not sure we're ever actually doing more than about 2
% reads, so it may not be worth it.
% 
% FIXME: According to the documentation for gen_tcp:recv/2 and gen_tcp:recv/3,
% they return *exactly* Length bytes, not at most Length bytes. If this is so,
% then this function is redundant and it should just call gen_tcp:recv. But I'm
% afraid to test it, because it's the sort of thing that even if it works now,
% might just stop working arbitrarily at some later point. We'd have to test it
% in a situation where there aren't actually that many bytes to read.
% 
% As a note, the gen_tcp:recv documentation is unclear about what happens if
% the timeout is hit. Empirically, it returns {error, timeout} in that case.
recv_with_length(Socket, Length, Timeout) ->
    recv_with_length_helper(Socket, Length, Timeout, <<>>).

recv_with_length_helper(Socket, Length, Timeout, Binary) ->
    case size(Binary) of
        Length ->
            % We've read exactly the right amount.
            {ok, Binary}
    ;
        Size when Size < Length ->
            % The timeout is per call and not in total. I'm not sure if this is
            % what we want, but it's easier to implement.
            NextResult = gen_tcp:recv(Socket, Length - Size, Timeout),
            case NextResult of
                {ok, Binary2} ->
                    % Concatenate the new data onto the old data; continue
                    % receiving.
                    CombinedBinary = <<Binary/binary, Binary2/binary>>,
                    recv_with_length_helper(Socket, Length, Timeout,
                                            CombinedBinary)
            ;
                % Propagate errors.
                SomethingElse ->
                    SomethingElse
            end
    end.

