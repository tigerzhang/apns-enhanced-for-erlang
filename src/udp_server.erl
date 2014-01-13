%% @author Zhang Hu <iamzhanghu@gmail.com>
%% @doc udp interface.

-module(udp_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).

-behaviour(gen_server).

-record(state, {
  socket          :: tuple(),
  buffer = <<>>   :: binary()
  }).

-define(SERVER_PORT, 2222).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, null, []).

stop() ->
  gen_server:cast(?MODULE, stop).

init(_Params) ->
  {ok, Sock} = gen_udp:open(?SERVER_PORT, [binary]),
  io:format("Init Sock ~p~n", [Sock]),
  {ok, #state{socket = Sock}}.

terminate(Reason, State) ->
  gen_udp:close(State#state.socket).

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({udp, Client, _Ip, _Port, Msg}, State = #state{buffer = CurrentBuffer}) ->
  io:format("receive udp data ~p from ~p~n", [Msg, Client]),
%  io:format("State[~p]~n", [State]),
  case <<CurrentBuffer/binary, Msg/binary>> of
    <<_Data1:31/binary, Uid:4/big-unsigned-integer-unit:8, _Data2:39/binary, MsgPayload/binary>> ->
      io:format("Uid[~p]~n", [Uid]),
      case MsgPayload of
        <<ApnPayloadLen:2/big-unsigned-integer-unit:8, Rest1/binary>> ->
          ApnPayload = binary:part(Rest1, 0, ApnPayloadLen),
          io:format("ApnPayloadLen len[~p] ApnPayload[~p]~n", [ApnPayloadLen, ApnPayload]),
          Rest2 = binary:part(Rest1, ApnPayloadLen, byte_size(Rest1) - ApnPayloadLen),
          io:format("Rest2 [~p]~n", [Rest2]),
          % skip 2 int4
          Rest21 = binary:part(Rest2, 8, byte_size(Rest2) - 8),
          io:format("Rest21 [~p]~n", [Rest21]),
          case Rest21 of
            <<AppkeyLen:2/big-unsigned-integer-unit:8, Rest3/binary>> ->
              Appkey = binary:part(Rest3, 0, AppkeyLen),
              io:format("AppkeyLen len[~p] Appkey[~p]~n", [AppkeyLen, Appkey]),
              Rest4 = binary:part(Rest3, AppkeyLen, byte_size(Rest3) - AppkeyLen),
              case erlang:size(Rest4) of
                0 -> {noreply, State#state{buffer = <<>>}};
                _ -> handle_info({udp, Client, _Ip, _Port, Rest4}, State#state{buffer = <<>>})
              end;
            _Other ->
              {noreply, State#state{buffer = <<>>}}
          end;
        _Other2 ->
          {noreply, State#state{buffer = <<>>}}
      end;
    _Other3 ->
      {noreply, State#state{buffer = <<>>}}
  end.

send_apn_message() ->
  noop.
  % try
  %   [Name, DeviceToken, Alert, Badge1, Sound, Expiry1, ExtraArgs] = Args,
  %   MngId = list_to_atom(Name),
  %   apns_manager:start_manager(MngId),
  %   {Badge, []} = string:to_integer(Badge1),
  %   {Expiry2, []} = string:to_integer(Expiry1),  
  %   Expiry = apns:expiry(Expiry2),
  %   {_, _, MicroSecs} = erlang:now(),
  %   {struct, ExtraArgsJson} = mochijson2:decode(ExtraArgs),
  %   apns_manager:send_message(MngId, apns:message_id(), DeviceToken, Alert,
  %     Badge, Sound, Expiry, ExtraArgsJson)
  % catch
  %   _:{Error, Reason} -> gen_tcp:send(Socket, io_lib:format("error ~p~n", [{Error, Reason}]))
  % end.

% handle_info(Msg, State) ->  
%   io:format("receive info ~p~n", [Msg]),
%   {noreply, State}.
