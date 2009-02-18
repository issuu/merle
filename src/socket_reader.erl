-module(socket_reader).
-author("Attila Babo").
-license("http://www.opensource.org/licenses/mit-license.php").
-export([wrap/1]).
-define(TIMEOUT, 5000).

%% @public
%% returns a wrapper function to read from the socket
%% valid parameters of the wrapper:
%%      start -> returns initial state
%%      peek -> returns a buffered chunk
%%      more -> tries to read and buffer some more input
%%      {take, N} -> returns N bytes consumed
wrap(Socket) ->
    input_s(start, Socket, <<>>).

%% @private
%% @doc proxy function for buffering input from a socket
input_s(Param, Socket, Buffer) ->
    case
        case Param of
            start -> Buffer;
            peek ->
                case Buffer of
                    <<>> ->
                        case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
                            {ok, New} -> New;
                            {error, timeout} -> {timeout, <<>>};
                            Error -> Error
                        end;
                    _ ->
                        Buffer
                end;
            more ->
                case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
                    {ok, New} -> <<Buffer/binary, New/binary>>;
                    {error, timeout} -> {timeout, Buffer};
                    Error -> Error
                end;
            {take, N} ->
                Size = size(Buffer),
                case Size >= N of
                    true->
                        split_binary(Buffer, N);
                    false ->
                        case gen_tcp:recv(Socket, N-Size, ?TIMEOUT) of
                            {ok, New} ->
                                {<<Buffer/binary, New/binary>>, <<>>};
                            {error, timeout} ->
                                {timeout, Buffer}
                        end
                end
        end
    of
        Continue when is_binary(Continue) ->
            {Continue, fun(X) -> input_s(X, Socket, Continue) end};
        {Return, Continue} when is_binary(Continue) ->
            {Return, fun(X) -> input_s(X, Socket, Continue) end};
        Other -> Other
    end.
