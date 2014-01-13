%% @author Zhang Hu <iamzhanghu@gmail.com>
%% @doc @todo Add description to udp_server_sup.


-module(udp_server_sup).
-behaviour(supervisor).
-export([init/1, start_child/0]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | shutdown | term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
  supervisor:start_child(?MODULE, []).

%% -spec start_child() -> {ok, term()}.	
%% start_child() ->
%% 	supervisor:start_child(?MODULE, []).

%% ====================================================================
%% Application Behavioural functions 
%% ====================================================================
%%start(_Type, _Args) ->
%%	supervisor:start_link(?MODULE, []).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    Connection = {connection,{udp_server,start_link, []},
	          permanent,2000,worker,[udp_server]},
    {ok,{{simple_one_for_one,20,10}, [Connection]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


