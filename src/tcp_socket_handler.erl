-module(tcp_socket_handler).
-behaviour(gen_server).
-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {msgid = 0 ::integer(), socket, auth}).

%%---------------------------------------------------------------------
%% Generic Server API
%%---------------------------------------------------------------------
start(_) ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  pg2:join(socket_handlers, self()),
  {ok, #state{}}.

%%---------------------------------------------------------------------
%% Internal API
%%---------------------------------------------------------------------
socket_init(Socket) ->
   %% Second number after socket is timeout; zero might still timeout on local connections.
   %% case gen_tcp:recv(Socket, 0, 0) of
   %% {ok, _Data} -> gen_tcp:send(Socket, "Erlang Test Protocol Server\nProtocol error, no data allowed before banner, closing connection.\n"), lib_protocol:close_socket(Socket);
   %% {error,timeout} -> ok;
   %% {error, closed} -> exit(normal), ok
   %% end,
   gen_tcp:send(Socket, "Erlang Test Protocol Server\nType help to display a list the commands\n"),
   inet:setopts(Socket, [{active, true}]).

%%---------------------------------------------------------------------
%% Generic Server Callback Functions
%%---------------------------------------------------------------------
handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

handle_cast({transfer, NewSocket, NewAuth}, State) ->
  socket_init(NewSocket),
  NewState = State#state{socket=NewSocket, auth=NewAuth},
  {noreply, NewState};

handle_cast({auth, NewAuth}, State) ->
  NewState = State#state{auth=NewAuth},
  {noreply, NewState};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({tcp, Socket, Data}, State) when State#state.auth =:= noauth ->
  T1=string:tokens(Data, "\r\n"),
  [lib_protocol:login(Socket, State, X)||X <- T1],
  {noreply, State};

%% handle_info({tcp, Socket, Data}, State) when State#state.auth =:= auth ->
%%   T1=string:tokens(Data, "\r\n"),
%%   [lib_protocol:protocol(Socket, State, X)||X <- T1],
%%   {noreply, State};

handle_info({tcp, Socket, Data}, State) when State#state.auth =:= auth ->
  T1=string:tokens(Data, "\r\n"),
  [{handle_wrapper(Socket, State, X), State#state{msgid = State#state.msgid + 1}} ||X <- T1],
  {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
  exit(normal),
  io:format("Client[~p] closed.~p", State#state.socket),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


len([]) -> 0;
len([_|T]) -> 1 + len(T).

handle_wrapper(Socket, State, Cmd) ->
	case string:tokens(Cmd, " ") of
		[Command] -> lib_protocol:protocol(Socket, State, Command);
		[Command | Args] -> lib_protocol:protocol(Socket, State, Command, Args)
	end.
