%% @author Zhang Hu <iamzhanghu@gmail.com>
%% @doc Apns Manager applicaion.


-module(apns_manager_app).
-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([stop/1, start/0, start/2]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
 	apns_manager_sup:start_link().

start() ->
	apns_manager_sup:start_link().

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


