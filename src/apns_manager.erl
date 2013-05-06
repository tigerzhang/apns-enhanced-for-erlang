%% @author zhanghu
%% @doc @todo Add description to apns_manager.

-module(apns_manager).
-behaviour(gen_server).

-include("apns.hrl").
-include("localized.hrl").

-record(state, {
				current_msg_pos = 1 :: integer(),	%% for performance concern, pos is counted from tail
				messages_sent :: list({binary(), apns_msg()}),
				manager_id :: atom()}).
-type state() :: #state{}.

-define(INTERVAL, 10000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, send_message/8, handle_call/3, handle_cast/2,
		 handle_info/2, init/1, terminate/2, code_change/3]).
-export([start_manager/1]).

-export([manager_id_to_connection_id/1, connection_id_to_manager_id/1]).

%% start_link/1
-spec start_link([]) -> {ok, pid()} | {error, {already_started, pid()}}.
start_link(MngId) ->
	gen_server:start_link({local, MngId}, ?MODULE, MngId, []).

start_manager(MngId) ->
	apns_manager_sup:start_manager(MngId).
	
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(MngId) ->
	io:format("apns_manager init ~p~n", [atom_to_list(MngId)]),
	apns:connect(manager_id_to_connection_id(MngId), fun log_error/3, fun log_feedback/1),
	erlang:send_after(?INTERVAL, self(), trigger),
	{ok, #state{manager_id=MngId, messages_sent=[]}}.

handle_cast({sendmsg, MngId, MsgId, DeviceToken, Alert, Badge, Sound, Expiry, ExtraArgs}, State) ->
	MessagesSent = [{MsgId, #apns_msg{id=MsgId, expiry=Expiry, device_token=DeviceToken, alert=Alert,
	 							 badge=Badge, sound=Sound, extra=ExtraArgs}, 
	 							 time_in_second()} | State#state.messages_sent],
	State1 = State#state{messages_sent = MessagesSent},
	gen_server:cast(MngId, sendmsg),
	{noreply, State1};

handle_cast(sendmsg, State) ->
	% try
		CurrentMsgPos = State#state.current_msg_pos,
		Len = erlang:length(State#state.messages_sent),
		if 
			Len < CurrentMsgPos ->
				{noreply, State};
			true ->
				%% message to send
				RevertMessagesSent = lists:reverse(State#state.messages_sent),

				{Key, Msg, _} = lists:nth(CurrentMsgPos, RevertMessagesSent),

				io:format("current_msg_pos ~p count of messages ~p~n", [CurrentMsgPos, Len]),

				%% calc the corresponding connection id
				MngId = State#state.manager_id,
				ConnId = manager_id_to_connection_id(MngId),

				%% send the message out
				io:format("try to connect: ~p~n",
					[{ConnId, apns:connect(ConnId, fun log_error/3, fun log_feedback/1)}]),
				case erlang:whereis(ConnId) of
					undefined ->
						throw(try_to_connnect_failed);
					Pid ->
						case erlang:is_process_alive(Pid) of
							true ->
								io:format("apns_connection alive: ~p ~p~n", [ConnId, Pid]);
							false ->
								io:format("apns_connection not alive:, ~p ~p~n", [ConnId, Pid])
						end
				end,

				apns:send_message(ConnId, Msg),
				MessagesSent2 = lists:keyreplace(Key, 1, State#state.messages_sent, {Key, Msg, time_in_second()}),

				NextMessagePos = CurrentMsgPos + 1,

				State1 = State#state{current_msg_pos = NextMessagePos, messages_sent = MessagesSent2},
				check_queued_messages(State1),

				{noreply, State1}
		end
	% catch
	% 	_:_ ->
	% 		io:format("handle_cast error"),
	% 		{noreply, State#state{current_msg_pos = 1, messages_sent = []}}
	% end
	;

handle_cast({rewind_position, MsgId, ConnPid}, State) ->
	io:format("rewind~n"),
	supervisor:terminate_child(apns_sup, ConnPid),

	RevertMessagesSent = lists:reverse(State#state.messages_sent),
	Pos = State#state.current_msg_pos,
	case find_message_id_backward(RevertMessagesSent, Pos, MsgId) of
		{ok, RewindPos} ->
			%% rewind to next position
			if
				RewindPos < erlang:length(State#state.messages_sent) ->
					io:format("rewind to ~p~n", [RewindPos + 1]),
					gen_server:cast(self(), sendmsg),
					{noreply, State#state{current_msg_pos = RewindPos + 1}};
				true ->
					{noreply, State}
			end;
		{error, not_found} ->
			io:format("rewind, message id ~p not found. go back to the header~n", [MsgId]),
			{noreply, State#state{current_msg_pos = 1}}
	end;
handle_cast(stop, State) ->
  {stop, normal, State}.

find_message_id_backward(Messages, CurPos, MsgId) ->
	Len = erlang:length(Messages),
	{Pos, {_, Msg, _}} = 
	if
		CurPos > Len ->
			{Len, lists:nth(Len, Messages)};
		CurPos < 1 ->
			{error, not_found};
		true ->
			{CurPos, lists:nth(CurPos, Messages)}
	end,
	io:format("current pos: ~p message id: ~p, target message id: ~p~n", [Pos, Msg#apns_msg.id, MsgId]),
	if 
		Msg#apns_msg.id == MsgId ->
			{ok, Pos};
		Pos == 1 ->
			{error, not_found};
		true ->
			find_message_id_backward(Messages, Pos - 1, MsgId)
	end.

send_message(MngId, MsgId, DeviceToken, Alert, Badge, Sound, Expiry, ExtraArgs) ->
	gen_server:cast(MngId, {
		sendmsg, MngId, MsgId,
		DeviceToken, Alert, Badge, Sound, Expiry, ExtraArgs}).

%% @hidden
-spec handle_call(X, reference(), state()) -> {stop, {unknown_request, X}, {unknown_request, X}, state()}.
handle_call(stop, _From, State) ->
	{stop, normal, shutdown_ok, State};
handle_call(Request, _From, State) ->
  {stop, {unknown_request, Request}, {unknown_request, Request}, State}.

-spec handle_info({ssl, tuple(), binary()} | {ssl_closed, tuple()} | X, state()) -> {noreply, state()} | {stop, ssl_closed | {unknown_request, X}, state()}.
handle_info(trigger, State) ->
	%% try to remove old messages
	RevertMessagesSent = lists:reverse(State#state.messages_sent),
	MessageTryToDeleteInOneLoop = 1,

	%% delete the message and adjust the cursor
	{RemainLoopCycle, RevertMessagesSent2} = do_remove_head_old_message(RevertMessagesSent, MessageTryToDeleteInOneLoop),
	MessageDeletedCount = MessageTryToDeleteInOneLoop - RemainLoopCycle,
	MessagesSent2 = lists:reverse(RevertMessagesSent2),
	CurPos2 = State#state.current_msg_pos - MessageDeletedCount,

	Pid = self(),
	Len = erlang:length(MessagesSent2),
	io:format("~p remove old message. current_msg_pos ~p count of messages ~p~n",
		[Pid, CurPos2, Len]),

	if
		Len == 0 ->
			erlang:send_after((?INTERVAL) * 10, Pid, trigger);
		Len > 0 ->
			erlang:send_after(?INTERVAL, Pid, trigger)
	end,
	{noreply, State#state{messages_sent = MessagesSent2, current_msg_pos = CurPos2}};
handle_info(Request, State) ->
  {stop, {unknown_request, Request}, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) -> 
	ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
log_error(MsgId, Status, Pid) ->
	{_, ConnId} = erlang:process_info(Pid, registered_name),
	MngId = connection_id_to_manager_id(ConnId),
	gen_server:cast(MngId, {rewind_position, MsgId, Pid}),
 	error_logger:error_msg("Error on msg ~p: ~p ~p~n", [MsgId, Status, ConnId]).
  
log_feedback(Token) ->
  error_logger:warning_msg("Device with token ~p removed the app~n", [Token]).

-spec manager_id_to_connection_id(atom()) -> atom().
manager_id_to_connection_id(MngId) ->
	MngIdBinary = erlang:atom_to_binary(MngId, latin1),
	ConnIdBinary = <<MngIdBinary/binary, <<"_conn">>/binary>>,
	erlang:binary_to_atom(ConnIdBinary, latin1).

-spec connection_id_to_manager_id(atom()) -> atom().
connection_id_to_manager_id(ConnId) ->
	ConnIdBinary = erlang:atom_to_binary(ConnId, latin1),
	Pos = erlang:byte_size(ConnIdBinary) - erlang:byte_size(<<"_conn">>),
	{MngIdBinary, <<"_conn">>} = erlang:split_binary(ConnIdBinary, Pos),
	erlang:binary_to_atom(MngIdBinary, latin1).

%% @doc make sure we have enought mailbox message to send the messages
-spec check_queued_messages(state()) -> ok.
check_queued_messages(State) ->
	{message_queue_len, MailboxQueueLen} = erlang:process_info(self(), message_queue_len),
	MessageQueueLen = erlang:length(State#state.messages_sent),
	MailboxMessageNeeded = (MessageQueueLen - State#state.current_msg_pos + 1),
	io:format("mailbox length ~p, message needed ~p~n", [MailboxQueueLen, MailboxMessageNeeded]),

	if 
		MailboxMessageNeeded > MailboxQueueLen ->
			gen_server:cast(self(), sendmsg);
		true ->
			noop
	end,
	ok.

time_in_second() ->
	calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

do_remove_head_old_message(Messages, MaxInOneLoop) ->
	Len = erlang:length(Messages),
	if
		MaxInOneLoop == 0 ->
			{MaxInOneLoop, Messages};
		Len =< 0 ->
			{MaxInOneLoop, Messages};
		true ->
			[Head | Tail] = Messages,
			{MsgId, _, Second} = Head,
			Now = time_in_second(),
			DeadLine = Second + (?INTERVAL/1000),
			io:format("Now ~p DeadLine ~p~n", [Now, DeadLine]),
			if
				Now > DeadLine->
					io:format("remove old message: ~p~p~n", [MsgId, Second]),
					do_remove_head_old_message(Tail, MaxInOneLoop - 1);
				true ->
					io:format("remove old message: message ~p is still alive, give up.~n", [MsgId]),
					{MaxInOneLoop, Messages}
			end
	end.