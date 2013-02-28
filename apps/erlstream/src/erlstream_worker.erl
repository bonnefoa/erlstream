%% @author Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>
%% @copyright 2012 Anthonin Bonnefoy.
%% @doc The stream worker.


-module(erlstream_worker).
-author('Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>').

-include("erlstream.hrl").
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link(FileName) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

init(FileName) ->
  error_logger:info_msg("Worker init with FileName: ~p~n", [FileName]),
  StreamConfiguration = parse_config:parse_stream_conf(FileName),
  proc_lib:spawn_link(twitter_stream, init_listen, [self(), StreamConfiguration]),
  {ok, CounterPid} = counter_serv:start_link(),
  %{ok, QueueMgrPid} = queue_manager:start_link(FileName),
  TimerPid = proc_lib:spawn_link(twitter_process, timer_count, [CounterPid]),
  {ok, #stream_state{counter_pid=CounterPid, timer_pid=TimerPid}}.

handle_cast({stream_start, Headers}, State) ->
  error_logger:info_msg("Handle_cast : ~p~n", [Headers]),
  {noreply, State};

handle_cast({stream, Part}, State) ->
  twitter_process:process_tweet(Part, State),
  {noreply, State};

handle_cast({stream_end, _Headers}, State) ->
  {stop,"Stream ended", State};

handle_cast(Msg, State) ->
  error_logger:info_msg("Handle_cast : ~p~n", [Msg]),
  {stop, Msg, State}.

handle_call(Request, _From, State) ->
  error_logger:error_msg("Handle_call : ~p~n", [Request]),
  Reply = ko,
  {stop,"No call allowed", Reply,State}.

handle_info(Info, State) ->
  error_logger:error_msg("Handle_info : ~p~n", [Info]),
  {stop,"Unexpected call", State}.

terminate(Reason, _State) ->
  error_logger:error_msg("Terminate : ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

