%%%-------------------------------------------------------------------
%%% @author zhanghu
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 四月 2014 上午12:30
%%%-------------------------------------------------------------------
-module(apns_mq_handler).
-author("zhanghu").

-include("apns.hrl").

-behaviour(gen_server2).

%% API
-export([start_link/0, test_package_from_mq/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    CBHost = get_env(token_cb_host, "ra-cb-1"),
    lager:log(info, self(), "connecting to cb: ~p", [CBHost]),
    case cberl:start_link(cberl_token, 5,
        CBHost,
        get_env(token_cb_username, ""),
        get_env(token_cb_passwd, "123456"),
        get_env(token_cb_name, "devicetoken")) of
        {error, Err} ->
            {failed, Err};
        {ok, _} ->
            ok
    end,

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({package_from_mq, ApnsPackage}, State) ->
    lager:log(info, self(), "apns package ~p", [ApnsPackage]),
    try
        <<_Uid:64/unsigned-big-integer,
        _Expiry:32/unsigned-big-integer,
        _PayloadLength:16/big,
        _BinPayload/binary>> = ApnsPackage
    of
        <<Uid:64/unsigned-big-integer, Expiry:32/unsigned-big-integer,
            PayloadLength:16/big, BinPayload/binary>> ->
            lager:log(info, self(), "Uid/Expiry/PayloadLength/BinPayload ~p/~p/~p/~p",
                [Uid, Expiry, PayloadLength, BinPayload]),

            %% TODO handle the every request in a seprate worker
            %% 在单独的 worker 里处理每一个请求
            Alert = BinPayload,
            case get_appkey_device_token(Uid) of
                {ok, Appkey, DeviceToken, TokenType} ->
                    lager:log(debug, self(), "Got from cb: Uid[~p] Device Token[~p]", [Uid, DeviceToken]),
                    CertKeyPath = get_cert_key_path(Appkey, TokenType),
                    ConnId = binary_to_atom(Appkey, latin1),
                    _Pid = apns:connect(ConnId, (apns:default_connection())#apns_connection
                    {
                        cert_file = binary_to_list(CertKeyPath),
                        error_fun = fun log_error/3,
                        feedback_fun = fun log_feedback/1
                    }),
                    DeviceTokenList = binary_to_list(DeviceToken),
                    apns:send_message(ConnId, DeviceTokenList, Alert, random:uniform(10), "chime");
                {error, GetDeviceTokenError} ->
                    lager:log(alert, self(), "Get device token failed Uid[~p] Error[~p]",
                        [Uid, GetDeviceTokenError])
            end
    catch
        Type:Error ->
            lager:log(error, self(), "catch ~p ~p", [Type, Error])
    end,
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_appkey_device_token(Uid) ->
    TokenKey = list_to_binary(integer_to_list(Uid)),
    case cberl:get(cberl_token, TokenKey) of
        {TokenKey, _ReturnedCasValue, TokenJson} ->
            try
                TokenRecord = apns_mochijson2:decode(binary_to_list(TokenJson)),
                Appkey = apns_mochijson2:get_value(<<"a">>,TokenRecord),
                %error_logger:info_msg("decode token record json succ:~p ,~n", [TokenRecord]),
                DeviceToken = apns_mochijson2:get_value(<<"t">>,TokenRecord),
                TokenType = case apns_mochijson2:get_value(<<"e">>,TokenRecord) of
                    <<"development">> ->
                        <<"1">>;
                    _ ->
                        <<"2">>
                end,
                {ok, Appkey, DeviceToken, TokenType}
            catch
                _:Err ->
                    lager:log(error, self(), "decode token record json error:~p ,~n", [Err]),
                    {error, Err}
            end;
        OtherResult ->
            lager:log(error, self(), "get device token error: ~p~n", [OtherResult]),
            {error, <<"token error">>}
    end.

get_env(K, Def) ->
    case application:get_env(apns_mng, K) of
        {ok, V} -> V;
        _ -> Def
    end.

-spec get_cert_key_path(Appkey :: binary(), TokenType :: binary()) -> binary().
get_cert_key_path(Appkey, TokenType) ->
    Dir = get_env(cert_key_dir, <<"./certs/">>),
    <<Dir/binary, Appkey/binary, <<"_">>/binary, TokenType/binary, <<".pem">>/binary>>.

log_error(MsgId, Status, Pid) ->
    lager:log(error, self(), "Error on msg ~p: ~p ~p~n", [MsgId, Status, Pid]).

log_feedback(Token) ->
    lager:log(warn, self(), "Device with token ~p removed the app~n", [Token]).

%% apns_mq_handler:test_package_from_mq(<<"你好9"/utf8>>).
test_package_from_mq(PayloadBin) when is_binary(PayloadBin)->
    Uid =2127006969017402752,
    Expiry = 86400,
    PayloadLength = 5,
%%     PayloadBin = PayloadBin,
    Package =
        <<Uid:64/unsigned-big-integer,
        Expiry:32/unsigned-big-integer,
        PayloadLength:16/big,
        PayloadBin/binary>>,
    gen_server:cast(?SERVER, {package_from_mq, Package}).