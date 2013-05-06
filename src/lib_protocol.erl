-module(lib_protocol).
-compile(export_all).
%% -import(tcp_socket_handler, [socket_login/1,socket_loop/1]).

-include("apns.hrl").
-record(state, {msgid = 0 ::integer(), socket, auth}).

%% -record(state, {socket, auth, nick, user, host, name}).

-define(TEST_CONNECTION, 'test-apnse').
-define(DEVICE_TOKEN, "130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49").

%%---------------------------------------------------------------------
%% Protocol handlers for not yet authorized connections.
%%---------------------------------------------------------------------
login(Socket, _State, "quit") ->
  close_socket(Socket);
login(Socket, _State, _) ->
  gen_tcp:send(Socket, "Protocol error, connection not authorized.\n").

%%---------------------------------------------------------------------
%% Protocol handlers for normal connections
%%---------------------------------------------------------------------
protocol(Socket, _State, "time") ->
  {Hour,Minute,Second} = erlang:time(),
  String = io_lib:format("Time: ~.2.0w:~.2.0w:~.2.0w~n", [Hour, Minute,Second]),
  gen_tcp:send(Socket, String);
protocol(Socket, _State, "count") ->
  Length=length(pg2:get_members(socket_handlers)),
  String = io_lib:format("Count: ~p~n", [Length]),
  gen_tcp:send(Socket, String);
protocol(Socket, _State, "data " ++ Data) ->
  String="Data: " ++ Data ++ "\r\n",
  gen_tcp:send(Socket, String);
protocol(Socket, _State, "self") ->
  Pid=self(),
  String=io_lib:format("PID: ~p~n", [Pid]),
  gen_tcp:send(Socket, String);
protocol(Socket, _State, "sleep") ->
  sleep(30000),
  close_socket(Socket);
protocol(Socket, _State, "status") ->
  String=io_lib:format("okay~n", []),
  gen_tcp:send(Socket, String);
protocol(Socket, _State, "quit") ->
  close_socket(Socket);
protocol(Socket, _State, "help") ->
	String = io_lib:format("apnse Name DeviceToken Alert
e.g.:
apnse PushTestDev1 130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49 hello11
apnse PushTestDev2 130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49 hello21
apnse PushTestDev1 130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49 hello12
apnse PushTestDev2 130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49 hello22

apnsm Name DeviceToken Alert Badge Sound Expiry ExtraArgs
e.g.:
apnsm PushTestDev3 130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49 hello31 31 chime 86400 {\"key\":31}
apnsm PushTestDev4 130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49 hello41 41 chime 86400 {\"key\":41}
apnsm PushTestDev3 130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49 hello32 32 chime 86400 {\"key\":32}
apnsm PushTestDev4 130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49 hello42 42 chime 86400 {\"key\":42}
", []),
	gen_tcp:send(Socket, String);
protocol(Socket, _State, "test1") ->
	apns:connect('test_conn1', (apns:default_connection())#apns_connection
				   { error_fun = fun log_error/3, feedback_fun = fun log_feedback/1 }),
%% 	Pid = case apns:connect(?TEST_CONNECTION, fun log_error/2, fun log_feedback/1) of
%% 		{ok, Pid} ->
%% 			Pid;
%% 		{error, {already_started, Pid}} ->
%% 			Pid
%% 	end,
	apns:send_message(?TEST_CONNECTION, ?DEVICE_TOKEN, "Test Alert", random:uniform(10), "chime"),
	gen_tcp:send(Socket, io_lib:format("apns enhanced test ~p~n", [self()]));
protocol(Socket, _State, Data) -> 
  io:format("Recieved: ~p~n", [Data]),
  gen_tcp:send(Socket, "Invalid command.\n").

protocol(Socket, _State, "apnse", Args) ->
	[Name, DeviceToken, Alert] = Args,
	ConnId = list_to_atom(Name),
	Pid = apns:connect(ConnId, (apns:default_connection())#apns_connection
				{ error_fun = fun log_error/3, feedback_fun = fun log_feedback/1 }),
	apns:send_message(ConnId, DeviceToken, Alert, random:uniform(10), "chime"),
	gen_tcp:send(Socket, io_lib:format("apns enhanced test ~p~n", [Pid]));
protocol(Socket, State, "apnsm", Args) ->
	[Name, DeviceToken, Alert, Badge1, Sound, Expiry1, ExtraArgs] = Args,
	MngId = list_to_atom(Name),
	apns_manager:start_manager(MngId),
	{Badge, []} = string:to_integer(Badge1),
	{Expiry2, []} = string:to_integer(Expiry1),
	Expiry = apns:expiry(Expiry2),
  {_, _, MicroSecs} = erlang:now(),
	apns_manager:send_message(MngId, apns:message_id(), DeviceToken, Alert, Badge, Sound, Expiry, [{<<"acme2">>, <<"x">>}]).

message_id() ->
  {_, _, MicroSecs} = erlang:now(),
  Secs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  First = Secs rem 65536,
  Last = MicroSecs rem 65536,
  First * 65536 + Last.

log_error(MsgId, Status, Pid) ->
  error_logger:error_msg("Error on msg ~p: ~p ~p~n", [MsgId, Status, Pid]).

log_feedback(Token) ->
  error_logger:warning_msg("Device with token ~p removed the app~n", [Token]).

%%---------------------------------------------------------------------
%% Generic function for properly closing the socket.
%%---------------------------------------------------------------------
close_socket(Socket) ->
  gen_tcp:close(Socket),
  exit(normal).

sleep(T) ->
    receive
    after T ->
       true
    end.

