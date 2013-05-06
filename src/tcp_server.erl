-module(tcp_server).
-export([start/0]).

%%---------------------------------------------------------------------
%% Application Start Function for use by erl -s
%%---------------------------------------------------------------------
start() -> application:start(tcp_server).

