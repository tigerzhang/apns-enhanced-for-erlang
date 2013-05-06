-module(apns_manager_tests).

-include("apns.hrl").
-include("localized.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").
-define(DUKE_PROD_TOKEN, "556890033300BD4140BECF44963CEBAA5C082784B507CB23C79B899D3CC1726A").
%% -define(DEVICE_TOKEN, "139D3CAB173FB230B97E4A19D288E3FBCD4B037F9B18ABA17FE4CDE72085E994").
%% -define(DEVICE_TOKEN, "ec516abdabcc233c7baed58c17572a0f32104a6d88f5c6177d8c8e1dd6030ad6").
%% -define(DEVICE_TOKEN, "130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49").
-define(DEVICE_TOKEN, "130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d48"). %% Wrong
%-define(DEVICE_TOKEN, "139D3CAB173AB230B97E4A19D288E3FBCD4B037F9B18ABA17FE4CDE72085E994"). %% Wrong

-define(TEST_CONNECTION, 'test-connection').

-export([main/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec main() -> no_return().
main() ->
  _ = application:load(apns),
  case eunit:test(apns_manager, [verbose]) of
    ok -> halt(0);
    _ -> halt(1)
  end.

-spec apns_manager_test_() -> {setup, fun(() -> ok), fun((_) -> ok), {timeout, 120000, fun(() -> any())}}.
apns_manager_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   {timeout, 120000, fun run/0}
   }.

%%% Tests
run() ->
  ?assertEqual('test_conn', apns_manager:manager_id_to_connection_id('test')),
  receive
    {'DOWN', _, _, _, _} = DownMsg1 ->
      ?fail(DownMsg1);
    DownMsg1 ->
      ?fail(DownMsg1)
    after 1000 ->
      ok
  end,
  ?assertEqual('test', apns_manager:connection_id_to_manager_id('test_conn')),
  receive
    {'DOWN', _, _, _, _} = DownMsg2 ->
      ?fail(DownMsg2);
    DownMsg2 ->
      ?fail(DownMsg2)
    after 1000 ->
      ok
  end.

-spec test() -> any().
