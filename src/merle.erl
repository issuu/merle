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
    stats/0, stats/1, version/0, getkey/1, delete/2, set/4, add/4, replace/2,
    replace/4, cas/5, set/2, flushall/0, flushall/1, verbosity/1, add/2,
    cas/3, getskey/1, connect/0, connect/1, connect/2, delete/1, disconnect/0
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
stats(Args) when is_atom(Args)->
	stats(atom_to_list(Args));
stats(Args) ->
	gen_server2:call(?SERVER, {stats, {Args}}).

%% @doc retrieve memcached version
version() ->
	gen_server2:call(?SERVER, {version}).

%% @doc set the verbosity level of the logging output
verbosity(Args) when is_integer(Args) ->
	verbosity(integer_to_list(Args));
verbosity(Args)->
	case gen_server2:call(?SERVER, {verbosity, {Args}}) of
		["OK"] -> ok;
		RETURN -> RETURN
	end.

%% @doc invalidate all existing items immediately
flushall() ->
	case gen_server2:call(?SERVER, {flushall}) of
		["OK"] -> ok;
		RETURN -> RETURN
	end.

%% @doc invalidate all existing items based on the expire time argument
flushall(Delay) when is_integer(Delay) ->
	flushall(integer_to_list(Delay));
flushall(Delay) ->
	case gen_server2:call(?SERVER, {flushall, {Delay}}) of
		["OK"] -> ok;
		RETURN -> RETURN
	end.

%% @doc retrieve value based off of key
getkey(Key) when is_atom(Key) ->
	getkey(atom_to_list(Key));
getkey(Key) ->
	case gen_server2:call(?SERVER, {getkey,{Key}}) of
	    ["END"] -> undefined;
	    RETURN -> RETURN
	end.

%% @doc retrieve value based off of key for use with cas
getskey(Key) when is_atom(Key) ->
	getskey(atom_to_list(Key));
getskey(Key) ->
	case gen_server2:call(?SERVER, {getskey,{Key}}) of
	    ["END"] -> undefined;
	    RETURN -> RETURN
	end.

%% @doc delete a key
delete(Key) ->
	delete(Key, "0").

delete(Key, Time) when is_atom(Key) ->
	delete(atom_to_list(Key), Time);
delete(Key, Time) when is_integer(Time) ->
	delete(Key, integer_to_list(Time));
delete(Key, Time) ->
	case gen_server2:call(?SERVER, {delete, {Key, Time}}) of
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

set(Key, Flag, ExpTime, Value) when is_atom(Key) ->
	set(atom_to_list(Key), Flag, ExpTime, Value);
set(Key, Flag, ExpTime, Value) when is_integer(Flag) ->
    set(Key, integer_to_list(Flag), ExpTime, Value);
set(Key, Flag, ExpTime, Value) when is_integer(ExpTime) ->
    set(Key, Flag, integer_to_list(ExpTime), Value);
set(Key, Flag, ExpTime, Value) ->
	case gen_server2:call(?SERVER, {set, {Key, Flag, ExpTime, Value}}) of
	    ["STORED"] -> ok;
	    ["NOT_STORED"] -> not_stored;
	    RETURN -> RETURN
	end.

%% @doc Store a key/value pair if it doesn't already exist.
add(Key, Value) ->
	Flag = random:uniform(?RANDOM_MAX),
	add(Key, integer_to_list(Flag), "0", Value).

add(Key, Flag, ExpTime, Value) when is_atom(Key) ->
	add(atom_to_list(Key), Flag, ExpTime, Value);
add(Key, Flag, ExpTime, Value) when is_integer(Flag) ->
    add(Key, integer_to_list(Flag), ExpTime, Value);
add(Key, Flag, ExpTime, Value) when is_integer(ExpTime) ->
    add(Key, Flag, integer_to_list(ExpTime), Value);
add(Key, Flag, ExpTime, Value) ->
	case gen_server2:call(?SERVER, {add, {Key, Flag, ExpTime, Value}}) of
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
	case gen_server2:call(?SERVER, {replace, {Key, Flag, ExpTime, Value}}) of
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
	case gen_server2:call(?SERVER, {cas, {Key, Flag, ExpTime, CasUniq, Value}}) of
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
	gen_server2:call(?SERVER, {stop}),
	ok.

%% @private
start_link(Host, Port) ->
    gen_server2:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

%% @private
init([Host, Port]) ->
    gen_tcp:connect(Host, Port, ?TCP_OPTS).

handle_call({stop}, _From, Socket) ->
    {stop, requested_disconnect, Socket};

handle_call({stats}, _From, Socket) ->
    Reply = send_generic_cmd(Socket, iolist_to_binary([<<"stats">>])),
    {reply, Reply, Socket};

handle_call({stats, {Args}}, _From, Socket) ->
    Reply = send_generic_cmd(Socket, iolist_to_binary([<<"stats ">>, Args])),
    {reply, Reply, Socket};

handle_call({version}, _From, Socket) ->
    Reply = send_generic_cmd(Socket, iolist_to_binary([<<"version">>])),
    {reply, Reply, Socket};

handle_call({verbosity, {Args}}, _From, Socket) ->
    Reply = send_generic_cmd(Socket, iolist_to_binary([<<"verbosity ">>, Args])),
    {reply, Reply, Socket};

handle_call({flushall}, _From, Socket) ->
    Reply = send_generic_cmd(Socket, iolist_to_binary([<<"flush_all">>])),
    {reply, Reply, Socket};

handle_call({flushall, {Delay}}, _From, Socket) ->
    Reply = send_generic_cmd(Socket, iolist_to_binary([<<"flush_all ">>, Delay])),
    {reply, Reply, Socket};

handle_call({getkey, {Key}}, _From, Socket) ->
    Reply = send_get_cmd(Socket, iolist_to_binary([<<"get ">>, Key])),
    {reply, Reply, Socket};

handle_call({getskey, {Key}}, _From, Socket) ->
    Reply = send_get_cmd(Socket, iolist_to_binary([<<"gets ">>, Key])),
    {reply, Reply, Socket};

handle_call({delete, {Key, Time}}, _From, Socket) ->
    Reply = send_generic_cmd(
        Socket,
        iolist_to_binary([<<"delete ">>, Key, <<" ">>, Time])
    ),
    {reply, Reply, Socket};

handle_call({set, {Key, Flag, ExpTime, Value}}, _From, Socket) ->
	Bin = to_binary(Value),
	Bytes = integer_to_list(size(Bin)),
    Reply = send_storage_cmd(
        Socket,
        iolist_to_binary([
            <<"set ">>, Key, <<" ">>, Flag, <<" ">>, ExpTime, <<" ">>, Bytes
        ]),
        Bin
    ),
    {reply, Reply, Socket};

handle_call({add, {Key, Flag, ExpTime, Value}}, _From, Socket) ->
	Bin = to_binary(Value),
	Bytes = integer_to_list(size(Bin)),
    Reply = send_storage_cmd(
        Socket,
        iolist_to_binary([
            <<"add ">>, Key, <<" ">>, Flag, <<" ">>, ExpTime, <<" ">>, Bytes
        ]),
        Bin
    ),
    {reply, Reply, Socket};

handle_call({replace, {Key, Flag, ExpTime, Value}}, _From, Socket) ->
	Bin = to_binary(Value),
	Bytes = integer_to_list(size(Bin)),
    Reply = send_storage_cmd(
        Socket,
        iolist_to_binary([
            <<"replace ">>, Key, <<" ">>, Flag, <<" ">>, ExpTime, <<" ">>,
            Bytes
        ]),
    	Bin
    ),
    {reply, Reply, Socket};

handle_call({cas, {Key, Flag, ExpTime, CasUniq, Value}}, _From, Socket) ->
	Bin = to_binary(Value),
	Bytes = integer_to_list(size(Bin)),
    Reply = send_storage_cmd(
        Socket,
        iolist_to_binary([
            <<"cas ">>, Key, <<" ">>, Flag, <<" ">>, ExpTime, <<" ">>, Bytes,
            <<" ">>, CasUniq
        ]),
        Bin
    ),
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
%% @doc send_generic_cmd/2 function for simple informational and deletion commands
send_generic_cmd(Socket, Cmd) ->
    gen_tcp:send(Socket, <<Cmd/binary, "\r\n">>),
    recv_simple_reply(Socket).

%% @private
%% @doc send_storage_cmd/3 funtion for storage commands
send_storage_cmd(Socket, Cmd, Value) ->
    gen_tcp:send(Socket, <<Cmd/binary, "\r\n">>),
    gen_tcp:send(Socket, <<Value/binary, "\r\n">>),
    recv_simple_reply(Socket).

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
%% @doc send_get_cmd/2 function for retreival commands
send_get_cmd(Socket, Cmd) ->
    gen_tcp:send(Socket, <<Cmd/binary, "\r\n">>),
	recv_complex_get_reply(Socket).

%% @private
%% @doc receive function for respones containing VALUEs
recv_complex_get_reply(Socket) ->
    recv_complex_get_reply(socket_reader:wrap(Socket), []).

recv_complex_get_reply({_, SocketReader}, Tail) ->
    recv_complex_get_reply(SocketReader, Tail);
recv_complex_get_reply(SocketReader, Tail) when is_function(SocketReader) ->
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
                    recv_complex_get_reply(Cont_3({take, 2}), [{list_to_tuple([Key, Flag | CAS]), Data} | Tail]);
                _ ->
                    recv_complex_get_reply(Cont_1(more), Tail)
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
