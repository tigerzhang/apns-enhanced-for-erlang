%% @author zhanghu
%% @doc @todo Add description to tcp_server_tests.


-module(tcp_server_tests).

-include("apns.hrl").
-include("localized.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/0]).

-spec main() -> no_return().
main() ->
	_ = application:load(apns),
	_ = application:load(tcp_server),
	case eunit:test(tcp_server, [verbose]) of
		ok -> halt(0);
		_ -> halt(1)
	end.

-spec tcp_server_test_() -> {setup, fun(() -> ok), fun((_) -> ok), {timeout, 120000, fun(() -> any())}}.
tcp_server_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) ->
           ?assertEqual(ok, apns:stop())
   end,
   {timeout, 120000, fun run/0}
   }.

run() ->
	io:format("tcp_server_tests:run~n"),
	apns:start(),
	tcp_server:start(),
	timer:sleep(2000),
	{ok, Socket} = gen_tcp:connect('127.0.0.1', 2222, []),
	gen_tcp:send(Socket, "apnse\r\n"),
	timer:sleep(2000),
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			io:format("recv ~p~n", [Packet]);
		{error, Reason} ->
			io:format("recv ~p~n", [Reason]);
		Other ->
			io:format("recv ???~n")
	end,
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


