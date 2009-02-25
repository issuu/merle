-module(merle_main).
-author("ab@issuu.com").
-behaviour(supervisor).
-behaviour(application).
-export([init/1]).
-export([start/2, stop/1]).

%% gen_server
start(_Type, _Args) ->
    Host = case application:get_env(memcached_host) of {ok, H} -> H; undefined -> {127,0,0,1} end,
    Port = case application:get_env(memcached_port) of {ok, P} -> P; undefined -> 11211 end,
    supervisor:start_link(?MODULE, [Host, Port]).

stop(_State) ->
    ok.

%% supervisor
init([Host, Port]) ->
    Server = {merle_service, {gen_server2, start_link, [{local, merle}, merle, [Host, Port], []]}, permanent, 500, worker, [merle]},
    {ok, {{one_for_one, 15, 5}, [Server]}}.
