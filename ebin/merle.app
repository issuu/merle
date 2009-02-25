{application, merle,
    [   {description, "memcached client"},
        {vsn, "0.3"},
        {modules,
            [merle
            ,socket_reader
            ]},
        {registered, [merle]},
        {applications,
            [kernel
            ,stdlib
            ,gen_server2
            ]},
        {mod, {merle, []}}]}.
