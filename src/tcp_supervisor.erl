-module(tcp_supervisor).
-behaviour(application).

-export([start_tcp_accept_handler/0, start_tcp_socket_handler/0]).
-export([start/2, stop/1, init/1]).

%%---------------------------------------------------------------------
%% External API
%%---------------------------------------------------------------------
start_tcp_accept_handler() ->
  supervisor:start_child(tcp_accept_supervisor, [tcp_accept_handler]).

start_tcp_socket_handler() ->
  supervisor:start_child(tcp_socket_supervisor, [tcp_socket_handler]).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
  pg2:create(accept_handlers),
  pg2:create(socket_handlers),
  supervisor:start_link(?MODULE, [apns_sup]),
  supervisor:start_link(?MODULE, [apns_manager_sup]),
  {ok, Pid} = supervisor:start_link(?MODULE, [tcp_listen_handler, 2222, 1024]),
  supervisor:start_link(?MODULE, [tcp_accept_supervisor]),
  supervisor:start_link(?MODULE, [tcp_socket_supervisor]),
  [start_tcp_accept_handler() || _ <- lists:seq(1,100)],
  {ok, Pid}.

stop(_S) ->
  ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([apns_sup]) ->
	{ok, {{one_for_one, 3, 10},
		  [{apns_sup, {apns_sup, start_link, []},
      transient, 5000, supervisor, [apns_sup]}
		  ]}};

init([apns_manager_sup]) ->
	{ok, {{one_for_one, 3, 10},
		  [{apns_manager_sup, {apns_manager_sup, start_link, []},
			transient, 5000, supervisor, [apns_manager_sup]}
		  ]}};

init([tcp_accept_supervisor]) ->
  io:format("Starting tcp_accept_supervisor..~n"),
  {ok, {{one_for_one, 3, 10},
  [{tcp_accept_supervisor, {supervisor,start_link,[{local, tcp_accept_supervisor}, ?MODULE, [tcp_accept_handler]]},
    permanent, 20000, supervisor, [tcp_accept_supervisor]}
  ]}};

init([tcp_socket_supervisor]) ->
  io:format("Starting tcp_socket_supervisor..~n"),
  {ok, {{one_for_one, 3, 10},
  [{tcp_socket_supervisor, {supervisor,start_link,[{local, tcp_socket_supervisor}, ?MODULE, [tcp_socket_handler]]},
    permanent, 20000, supervisor, [tcp_socket_supervisor]}
  ]}};

init([tcp_listen_handler, Port, MaxConn]) ->
  io:format("Starting tcp_listen_handler..~n"),
  {ok, {{one_for_one, 3, 10},
  [{tcp_listen_handler, {tcp_listen_handler,start,[Port,MaxConn]},
    permanent, 20000, worker, [tcp_listen_handler]}
  ]}};

init([tcp_accept_handler]) ->
  io:format("Starting tcp_accept_handler..~n"),
  {ok, {{simple_one_for_one, 100, 1},
  [{undefined, {tcp_accept_handler,start,[]},
    permanent, 20000, worker, []}
  ]}};

init([tcp_socket_handler]) ->
  io:format("Starting tcp_socket_handler..~n"),
  {ok, {{simple_one_for_one, 3, 10},
  [{undefined, {tcp_socket_handler,start,[]},
    temporary, 20000, worker, []}
  ]}}.

