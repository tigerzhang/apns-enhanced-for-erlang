%% @author zhanghu
%% @doc @todo Add description to apns_manager_sup.


-module(apns_manager_sup).
-behaviour(supervisor).
-export([init/1, start_manager/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start_link/1]).

-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | shutdown | term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_link(ConnId :: term()) -> {ok, pid()}
				| {error, {already_started, pid()}}.
start_link(ConnId) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [ConnId]).

start_manager(ConnId) ->
  supervisor:start_child(?MODULE, [ConnId]).

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
    Connection = {connection,{apns_manager,start_link, []},
	          permanent,2000,worker,[apns_manager]},
    {ok,{{simple_one_for_one,0,1}, [Connection]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


