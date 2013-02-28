%% @author Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>
%% @copyright 2012 Anthonin Bonnefoy.
%% @doc Simple counter server

-module(counter_serv).
-author('Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>').
-behaviour(gen_server).
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, 0}.

handle_cast({increment}, State) ->
  {noreply, State + 1};

handle_cast({display}, State) ->
  error_logger:info_msg("Count:~p~n",[State]),
  {noreply, 0};

handle_cast(Msg, State) -> {stop, Msg, State}.

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

