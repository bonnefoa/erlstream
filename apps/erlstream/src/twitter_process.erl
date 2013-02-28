%% @author Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>
%% @copyright 2012 Anthonin Bonnefoy.
%% @doc The twitter processor.

-module(twitter_process).
-author('Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>').

-include("erlstream.hrl").
-export([ process_tweet/2, timer_count/1 ]).

process_tweet(BinJson, #stream_state{counter_pid=CounterPid}) ->
  {struct, PropList} = mochijson2:decode(BinJson),
  case proplists:lookup(<<"text">>, PropList) of
    { _, _Text } ->
      error_logger:info_msg("Message:~p~n",[BinJson]),
      gen_server:cast(CounterPid, {increment});
      %gen_server:cast(QueueMgrPid, {send_to_queue, BinJson});
    none ->
      error_logger:info_msg("Message:~p~n",[PropList]),
      ok
  end.


timer_count(CounterPid) ->
  receive
    _Any ->
      timer_count(CounterPid)
  after 1000 ->
      gen_server:cast(CounterPid, {display}),
      timer_count(CounterPid)
  end.

