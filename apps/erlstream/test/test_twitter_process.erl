-module(test_twitter_process).
-include("common_test.hrl").

setup() ->
  ok.

cleanup(_Pid) ->
  ok.


test_parse_json() ->
  fun() ->
      ok
  end .

generator_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      test_parse_json()
    ]
  }.
