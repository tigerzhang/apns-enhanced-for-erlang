-module(tcp_listen_handler).
-behaviour(gen_server).
-export([start/2, getstate/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, 
		{
		 listen :: gen_tcp:socket(),
		 maxconn :: integer()
		}).

%%---------------------------------------------------------------------
%% Generic Server API
%%---------------------------------------------------------------------
start(Port, MaxConn) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, MaxConn], []).

init([Port, MaxConn]) ->
  {ok, Listen} = gen_tcp:listen(Port, [list, inet6, {packet, 0}, {reuseaddr, true}, {nodelay, true}, {active, false}]),
  {ok, #state{listen=Listen, maxconn=MaxConn}}.

%%---------------------------------------------------------------------
%% External API
%%---------------------------------------------------------------------
getstate() -> gen_server:call(?MODULE, getstate).

%%---------------------------------------------------------------------
%% Generic Server Callback Functions
%%---------------------------------------------------------------------
handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call(getstate, _From, State) ->
  Listen=State#state.listen,
  MaxConn=State#state.maxconn,
  {reply, {Listen, MaxConn}, State};
handle_call(Request, _From, State) -> {reply, {reply, self(), Request}, State}.

handle_cast({newmax, NewMaxConn}, State) ->
  NewState = State#state{maxconn=NewMaxConn},
  {noreply, NewState};

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
