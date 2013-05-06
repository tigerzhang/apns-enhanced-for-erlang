%% @author zhanghu
%% @doc @todo Add description to apns_manager_connection.


-module(apns_manager_connection).
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle_info/2, handle_cast/2, handle_call/3, start_link/1]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {socket				:: tuple(),
				in_buffer = <<>>	:: binary(),
				out_buffer = <<>>	:: binary()}).

%% start_link/1
-spec start_link([]) -> {ok, pid()} | {error, {already_started, pid()}}.
start_link(Port) ->
	supervisor:start_link(?MODULE, Port),
	supervisor:start_link(?MODULE, accept).
	

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(Port) ->
	try
		case gen_tcp:listen(Port, []) of
			{ok, LSocket} -> {ok, #state{socket=LSocket}};
			{error, Reason} -> {stop, Reason}
		end
	catch
		_:{error, Reason2} -> {stop, Reason2}
	end;
init(accept) ->
	{ok, []}.
			
%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({tcp, Socket, RawData}, State) ->
	io:format("gen_server[~p]: handle_info:tcp ~n", [self()]),
	gen_tcp:send(Socket, "apns_manager_connect: " ++ RawData),
    {noreply, State};

handle_info(timeout, State) ->
%% 	io:format("gen_server[~p]: handle_info : timeout.accept ~n",[self()]),
%% 	{ok, _Sock} = gen_tcp:accept(State#state.socket),
%% 	io:format("gen_server[~p]: handle_info : timeout.accpeded incoming connection...",[self()]),
%% 	apns_manager_sup:start_child(),
	{noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    io:format("gen_server[~p]: handle_info : tcp closed ~n",[self()]),
    {stop, normal, State}.

%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
	io:format("terminate",[]),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


