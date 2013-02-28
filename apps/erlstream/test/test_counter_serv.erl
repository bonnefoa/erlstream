%% @author Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>
%% @copyright 2012 Anthonin Bonnefoy.
%% @doc Test for the simple counter server

-module(test_counter_serv).
-include("common_test.hrl").

setup() ->
  ok.

cleanup(_Pid) ->
  ok.

test_counter() ->
  fun() ->
      {ok, Pid} = counter_serv:start_link()
      , gen_server:cast(Pid, {increment})
      , gen_server:cast(Pid, {display})
  end .

generator_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      test_counter()
    ]
  }.

