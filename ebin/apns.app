{application,apns,
             [{description,"APNS Enhance for Erlang"},
              {vsn,"1.0"},
              {id,"apns_enhanced"},
              {modules,[apns,apns_app,apns_connection,apns_manager,
                        apns_manager_app,apns_manager_connection,
                        apns_manager_sup,apns_manager_tests,apns_mochijson2,
                        apns_mochinum,apns_sup,apns_tests,lib_protocol,
                        tcp_accept_handler,tcp_client,tcp_listen_handler,
                        tcp_server,tcp_server_tests,tcp_socket_handler,
                        tcp_supervisor]},
              {registered,[tcp_listen_handler,tcp_accept_supervisor,
                           tcp_socket_supervisor]},
              {applications,[kernel,stdlib,ssl]},
              {mod,{tcp_supervisor,[]}},
              {env,[{apple_host,"gateway.sandbox.push.apple.com"},
                    {apple_port,2195},
                    {cert_file,"priv/cert.pem"},
                    {key_file,undefined},
                    {timeout,30000},
                    {feedback_host,"feedback.sandbox.push.apple.com"},
                    {feedback_port,2196},
                    {feedback_timeout,600000}]}]}.