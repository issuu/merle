{application, merle,
    [   {description, "memcached client"},
        {vsn, "0.3"},
        {modules,
            [merle
            ,gen_server2
            ,socket_reader
            ]},
        {registered, [merle]},
        {applications,
            [kernel
            ,stdlib
            ]},
        {mod, {merle, []}}]}.
