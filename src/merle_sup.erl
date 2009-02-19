-module(merle_sup).
-author("ab@issuu.com").
-behaviour(supervisor).
-export([init/1]).

init([Host, Port]) ->
    Server = {merle_service, {gen_server2, start_link, [{local, merle}, merle, [Host, Port], []]}, permanent, 500, worker, [merle]},
    {ok, {{one_for_one, 15, 5}, [Server]}}.
