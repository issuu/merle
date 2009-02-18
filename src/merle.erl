%% Copyright 2009, Joe Williams <joe@joetify.com>
%% Copyright 2009, Nick Gerakines <nick@gerakines.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @author Joseph Williams <joe@joetify.com>
%% @copyright 2008 Joseph Williams
%% @version 0.2
%% @seealso http://code.sixapart.com/svn/memcached/trunk/server/doc/protocol.txt
%% @doc An Erlang memcached client.
%%
%% This code is available as Open Source Software under the MIT license.
%%
%% Updates at http://github.com/joewilliams/merle/

-module(merle).
-behaviour(gen_server2).

-author("Joe Williams <joe@joetify.com>").
-version("Version: 0.2").

-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).
-define(RANDOM_MAX, 65535).
-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 11211).
-define(TCP_OPTS, [
    binary, {packet, raw}, {nodelay, true},{reuseaddr, true}, {active, false}
]).

%% gen_server API
-export([
    stats/0, stats/1, version/0, get/1, delete/2, set/4, add/4, replace/2,
    replace/4, cas/5, set/2, flush_all/0, flush_all/1, verbosity/1, add/2,
    cas/3, gets/1, connect/0, connect/1, connect/2, delete/1, disconnect/0
]).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

%% @doc retrieve memcached stats
stats() ->
	gen_server2:call(?SERVER, {stats}).

%% @doc retrieve memcached stats based on args
stats(Args) ->
	gen_server2:call(?SERVER, {stats, Args}).

%% @doc retrieve memcached version
version() ->
	gen_server2:call(?SERVER, [version]).

%% @doc set the verbosity level of the logging output
verbosity(Arg) when is_integer(Arg) ->
	case gen_server2:call(?SERVER, {verbosity, [Arg]}) of
		["OK"] -> ok;
		RETURN -> RETURN
	end.

%% @doc invalidate all existing items immediately
flush_all() ->
	case gen_server2:call(?SERVER, {flush_all}) of
		["OK"] -> ok;
		RETURN -> RETURN
	end.

%% @doc invalidate all existing items based on the expire time argument
flush_all(Delay) when is_integer(Delay) ->
	case gen_server2:call(?SERVER, {flush_all, [Delay]}) of
		["OK"] -> ok;
		RETURN -> RETURN
	end.

%% @doc retrieve value based off of key
get(Key) ->
    gen_server2:call(?SERVER, {get, Key}).

%% @doc retrieve value based off of key for use with cas
gets(Key) ->
	gen_server2:call(?SERVER, {gets, Key}).

%% @doc delete a key
delete(Key) ->
	delete(Key, "0").

delete(Key, Time) ->
	case gen_server2:call(?SERVER, {delete, [Key, Time]}) of
		["DELETED"] -> ok;
		["NOT_FOUND"] -> not_found;
		RETURN -> RETURN
	end.

%% Time is the amount of time in seconds
%% the client wishes the server to refuse
%% "add" and "replace" commands with this key.

%%
%% Storage Commands
%%

%% *Flag* is an arbitrary 16-bit unsigned integer (written out in
%% decimal) that the server stores along with the Value and sends back
%% when the item is retrieved.
%%
%% *ExpTime* is expiration time. If it's 0, the item never expires
%% (although it may be deleted from the cache to make place for other
%%  items).
%%
%% *CasUniq* is a unique 64-bit value of an existing entry.
%% Clients should use the value returned from the "gets" command
%% when issuing "cas" updates.
%%
%% *Value* is the value you want to store.

%% @doc Store a key/value pair.
set(Key, Value) ->
    Flag = random:uniform(?RANDOM_MAX),
    set(Key, integer_to_list(Flag), "0", Value).

set(Key, Flag, ExpTime, Value) ->
	case gen_server2:call(?SERVER, {set, [Key, Flag, ExpTime], Value}) of
	    ["STORED"] -> ok;
	    ["NOT_STORED"] -> not_stored;
	    RETURN -> RETURN
	end.

%% @doc Store a key/value pair if it doesn't already exist.
add(Key, Value) ->
	Flag = random:uniform(?RANDOM_MAX),
	add(Key, integer_to_list(Flag), "0", Value).

add(Key, Flag, ExpTime, Value) ->
	case gen_server2:call(?SERVER, {add, [Key, Flag, ExpTime], Value}) of
	    ["STORED"] -> ok;
	    ["NOT_STORED"] -> not_stored;
	    RETURN -> RETURN
	end.

%% @doc Replace an existing key/value pair.
replace(Key, Value) ->
	Flag = random:uniform(?RANDOM_MAX),
	replace(Key, integer_to_list(Flag), "0", Value).

replace(Key, Flag, ExpTime, Value) when is_atom(Key) ->
	replace(atom_to_list(Key), Flag, ExpTime, Value);
replace(Key, Flag, ExpTime, Value) when is_integer(Flag) ->
    replace(Key, integer_to_list(Flag), ExpTime, Value);
replace(Key, Flag, ExpTime, Value) when is_integer(ExpTime) ->
    replace(Key, Flag, integer_to_list(ExpTime), Value);
replace(Key, Flag, ExpTime, Value) ->
	case gen_server2:call(?SERVER, {replace, [Key, Flag, ExpTime], Value}) of
	    ["STORED"] -> ok;
	    ["NOT_STORED"] -> not_stored;
	    RETURN -> RETURN
	end.

%% @doc Store a key/value pair if possible.
cas(Key, CasUniq, Value) ->
	Flag = random:uniform(?RANDOM_MAX),
	cas(Key, integer_to_list(Flag), "0", CasUniq, Value).

cas(Key, Flag, ExpTime, CasUniq, Value) when is_atom(Key) ->
	cas(atom_to_list(Key), Flag, ExpTime, CasUniq, Value);
cas(Key, Flag, ExpTime, CasUniq, Value) when is_integer(Flag) ->
    cas(Key, integer_to_list(Flag), ExpTime, CasUniq, Value);
cas(Key, Flag, ExpTime, CasUniq, Value) when is_integer(ExpTime) ->
    cas(Key, Flag, integer_to_list(ExpTime), CasUniq, Value);
cas(Key, Flag, ExpTime, CasUniq, Value) when is_integer(CasUniq) ->
    cas(Key, Flag, ExpTime, integer_to_list(CasUniq), Value);
cas(Key, Flag, ExpTime, CasUniq, Value) ->
	case gen_server2:call(?SERVER, {cas, [Key, Flag, ExpTime, CasUniq], Value}) of
	    ["STORED"] -> ok;
	    ["NOT_STORED"] -> not_stored;
	    RETURN -> RETURN
	end.

%% @doc connect to memcached with defaults
connect() ->
	connect(?DEFAULT_HOST, ?DEFAULT_PORT).

%% @doc connect to memcached with default port
connect(Host) ->
    connect(Host, ?DEFAULT_PORT).

%% @doc connect to memcached
connect(Host, Port) ->
	start_link(Host, Port).

%% @doc disconnect from memcached
disconnect() ->
	gen_server2:call(?SERVER, stop),
	ok.

%% @private
start_link(Host, Port) ->
    gen_server2:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

%% @private
init([Host, Port]) ->
    gen_tcp:connect(Host, Port, ?TCP_OPTS).

handle_call(stop, _From, Socket) ->
    {stop, requested_disconnect, Socket};

handle_call({Cmd}, _From, Socket) when is_atom(Cmd) ->
    Reply = send_generic_cmd(Socket, Cmd),
    {reply, Reply, Socket};

handle_call({Cmd, Args}, _From, Socket) when is_atom(Cmd) ->
    Reply = send_generic_cmd(Socket, Cmd, Args),
    {reply, Reply, Socket};

handle_call({Cmd, Args, Value}, _From, Socket) when is_atom(Cmd) ->
    Reply = send_generic_cmd(Socket, Cmd, Args, to_binary(Value)),
    {reply, Reply, Socket}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info(_Info, State) -> {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @private
%% @doc Closes the socket
terminate(_Reason, Socket) ->
    gen_tcp:close(Socket),
    ok.

%% @private
%% @doc format command in memcached style, add spaces and closing \r\n
cmd_format(Cmd) when is_list(Cmd) ->
    cmd_format(Cmd, []);
cmd_format(Cmd) when is_atom(Cmd) ->
    cmd_format([Cmd], []).

cmd_format([], []) ->
    <<>>;
cmd_format([], Ready) ->
    iolist_to_binary(lists:reverse([$\n, $\r | tl(Ready)]));
cmd_format([H|T], Ready) when is_atom(H) ->
    cmd_format(T, [$\s, atom_to_list(H) | Ready]);
cmd_format([H|T], Ready) when is_integer(H) ->
    cmd_format(T, [$\s, integer_to_list(H) | Ready]);
cmd_format([H|T], Ready) when is_list(H); is_binary(H) ->
    cmd_format(T, [$\s, H | Ready]).

%% @private
%% @doc generic function to send commands with optional arguments and value
send_generic_cmd(Socket, Cmd) ->
    gen_tcp:send(Socket, cmd_format(Cmd)),
    recv_simple_reply(Socket).

send_generic_cmd(Socket, Cmd, Args) when is_list(Args) ->
    gen_tcp:send(Socket, cmd_format([Cmd | Args])),
    case Cmd =:= get orelse Cmd =:= gets of
        true -> recv_complex_reply(Socket);
        false -> recv_simple_reply(Socket)
    end;
send_generic_cmd(Socket, Cmd, Args) ->
    send_generic_cmd(Socket, Cmd, [Args]).

send_generic_cmd(Socket, Cmd, Args, Value) when is_list(Args), is_binary(Value) ->
    Bytes = size(Value),
    {A, B} = lists:split(3, Args),
    gen_tcp:send(Socket, cmd_format([Cmd | A ++ [Bytes | B]])),
    gen_tcp:send(Socket, <<Value/binary, "\r\n">>),
    recv_simple_reply(Socket);
send_generic_cmd(Socket, Cmd, Args, Value) ->
    send_generic_cmd(Socket, Cmd, [Args], Value).

%% @private
%% @doc receive function for simple responses (not containing VALUEs)
recv_simple_reply(Socket) ->
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
	  	{ok, Data} ->
        	string:tokens(binary_to_list(Data), "\r\n");
        Error ->
  			Error
    end.

%% @private
%% @doc receive function for respones containing VALUEs
recv_complex_reply(Socket) ->
    recv_complex_reply(socket_reader:wrap(Socket), []).

recv_complex_reply({_, SocketReader}, Tail) ->
    recv_complex_reply(SocketReader, Tail);
recv_complex_reply(SocketReader, Tail) when is_function(SocketReader) ->
    case SocketReader(peek) of
        {<<"END\r\n">>, _} ->
            lists:reverse(Tail);
        {RCV, Cont_1} when is_binary(RCV), is_function(Cont_1) ->
            case re:run(RCV, "VALUE (\\w+) (\\d+) (\\d+)\s?(\\d+)?", [{capture, all, list}]) of
                {match, Match} ->
                    [Skip, Key, Flag, Len | CAS] =
                        zipf(
                            [fun(X) -> length(X)+2 end
                            ,fun list_to_binary/1
                            ,fun list_to_integer/1
                            ,fun list_to_integer/1
                            ,fun list_to_integer/1
                            ], Match),
                    {_, Cont_2} = Cont_1({take, Skip}),
                    {Data, Cont_3} = Cont_2({take, Len}),
                    recv_complex_reply(Cont_3({take, 2}), [{list_to_tuple([Key, Flag | CAS]), Data} | Tail]);
                _ ->
                    recv_complex_reply(Cont_1(more), Tail)
            end;
        Error -> Error
    end.

%% @private
%% @doc convert to binary all terms but binary
to_binary(X) when is_binary(X) ->
    X;
to_binary(X) ->
    term_to_binary(X).

%% @private
%% @doc apply a list of functions on a list of values, returns the resulting list
%% @doc list should be different lenght
zipf(F, V) when is_list(F), is_list(V) ->
    lists:reverse(zipf(F, V, [])).

zipf([], _, T) -> T;
zipf(_, [], T) -> T;
zipf([F|FT], [V|VT], T) ->
    zipf(FT, VT, [F(V) | T]).
