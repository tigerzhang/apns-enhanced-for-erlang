-module(tcp_accept_handler).
-behaviour(gen_server).
-export([start/1, change_max/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {listener, max}).

%%---------------------------------------------------------------------
%% Generic Server API
%%---------------------------------------------------------------------
-spec start(Args::term()) -> {ok,Pid::pid()} | ignore | {error,Error::term()}.
start(_) ->
  gen_server:start_link(?MODULE, [], []).

-spec init(Args::term()) -> {ok, State::term()}.
init([]) ->
  pg2:join(accept_handlers, self()),
  gen_server:cast(self(), accept),
  {Listen, Max} = tcp_listen_handler:getstate(),
  {ok, #state{listener=Listen, max=Max}}.

%%---------------------------------------------------------------------
%% External API
%%---------------------------------------------------------------------
change_max(Max) ->
  [gen_server:cast(X, {newmax, Max})||X <- pg2:get_members(accept_handlers)],
   gen_server:cast(tcp_listen_handler, {newmax, Max}).

%%---------------------------------------------------------------------
%% Generic Server Callback Functions
%%---------------------------------------------------------------------
handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call(Request, _From, State) -> {reply, {reply, self(), Request}, State}.

handle_cast(accept, State) ->
  Listen=State#state.listener,
  Max=State#state.max,
  case gen_tcp:accept(Listen) of
  {ok, Socket} ->
    Length=length(pg2:get_members(socket_handlers)),
    if Length < Max -> 
         {ok, Pid} = tcp_supervisor:start_tcp_socket_handler(),
         gen_tcp:controlling_process(Socket, Pid),
         gen_server:cast(Pid, {transfer, Socket, auth}),
         gen_server:cast(self(), accept);
       Length >= Max -> 
         gen_tcp:send(Socket, "Too many connections, closing.\n"),
         gen_tcp:close(Socket),
         gen_server:cast(self(), accept)
    end;
  {error, closed} -> exit(listenerClosed)
    end, {noreply, State};

handle_cast({newmax, NewMax}, State) ->
  NewState = State#state{max=NewMax},
  {noreply, NewState};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

