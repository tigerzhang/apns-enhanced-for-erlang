{application, tcp_server,
 [
  {description, "Erlang Test Protocol Server"},
  {vsn, "1.0"},
  {id, "tcp_server"},
  {modules,      [tcp_server, tcp_supervisor, tcp_listen_handler, tcp_socket_handler, tcp_accept_handler, lib_protocol]},
  {registered,   [tcp_listen_handler, tcp_accept_supervisor, tcp_socket_supervisor]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {tcp_supervisor, []}},
  {env, []}
 ]
}.
