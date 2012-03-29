{application,merle,
             [{description,"memcached client"},
              {vsn,"0.3"},
              {modules,[gen_server2,merle,merle_main,socket_reader]},
              {registered,[merle]},
              {applications,[kernel,stdlib]},
              {mod,{merle_main,[]}}]}.
