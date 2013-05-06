-module(tcp_client).
-export([start/0, show_count/0]).
-import(lists, [reverse/1]).

start() ->
[spawn(fun() -> tc(X) end) || X <- lists:seq(1,2000)],
spawn(fun() -> count() end),
spawn(fun() -> reporter(0) end).

show_count() -> count ! {ok, count}.

reporter(C) ->
sleep(1000),
count ! {self(), reporter},
receive {ok, C1} -> true end,
C2=C1-C,
io:format("Connections per second: ~p~n", [C2]),
%% io:format("Transactions per second: ~p~n", [C2*200]),
reporter(C1).

count() ->
Pid=self(),
register(count,Pid),
count({0,0,0,0}).

count({S,Fc,Ft,Fm}) ->
receive
  {ok, inc} -> count({S+1,Fc,Ft,Fm});
  {fail_closed, inc} -> count({S,Fc+1,Ft,Fm});
  {fail_timeout, inc} -> count({S,Fc,Ft+1,Fm});
  {fail_maxcon, inc} -> count({S,Fc,Ft,Fm+1});
  {From, reporter} -> From ! {ok, S}, count({S,Fc,Ft,Fm});
  {ok, count} -> io:format("Good connections: ~p.~n", [S]),
		 io:format("Maximum connections: ~p.~n", [Fm]),
		 io:format("Timed out connections: ~p.~n", [Ft]),
		 io:format("Unexpectedly closed connections: ~p.~n", [Fm]),
		 count({S,Fc,Ft,Fm})
end.

tc(X) ->
%% rsleep(50,1000),
sleep(X),
case gen_tcp:connect("127.0.0.1",2222,[inet6, {packet, 0}]) of
     {ok,Socket} -> 
	receive_data(Socket),
	ok = gen_tcp:send(Socket, "status\r\nquit\r\n"), receive_data(Socket),
	count ! {ok, inc};
     {error,Error} -> io:format("Got connect error: ~p~n", [Error]) end,
tc(1000).

receive_data(Socket) ->
receive
  {tcp,Socket,Data} -> process_data({ok,Data});
  {tcp_closed,Socket} -> count ! {fail_closed, inc};
  {Data} -> io:format("Got socket error: ~p~n", [Data])
  after 5000 -> count ! {fail_timeout, inc}
end.

process_data({ok,"okay\n"}) -> true;
process_data({ok,"Erlang Test Protocol Server\n"}) -> true;
process_data({ok,"Too many connections, closing.\n"}) -> count ! {fail_maxcon, inc};
process_data(_Data) -> true.

sleep(T) ->
    receive
    after T ->
       true
    end.

rsleep(L,H) ->
    {ok, Rand} = random_num(L,H),
    %% io:format("Sleeping ~p~n", [Rand]),
    sleep(Rand).

random_num(L,H) ->
     {_,X,Y} = erlang:now(),
     {_,_,Z} = time(),
     Base=H-L,
     R1 = X * Y * Z rem Base,
     R2 = R1+L,
     {ok, R2}.

