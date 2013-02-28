%% @author Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>
%% @copyright 2012 Anthonin Bonnefoy.
%% @doc Manager for message queue

-module(queue_manager).
-author('Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>').

-include("erlstream.hrl").
-include("amqp_client.hrl").
-behaviour(gen_server).
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(FileName) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

init(FileName) ->
  BrokerConf = parse_config:parse_broker(FileName),
  {ok, Connection} = amqp_connection:start(BrokerConf),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  Exchange = parse_config:parse_exchange(FileName),
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
  Queue = parse_config:parse_queue_declare(FileName),
  #'queue.declare_ok'{} = amqp_channel:call(Channel, Queue),
  RoutingKey = "routing_key",
  Binding = #'queue.bind'{
    queue       = Queue,
    exchange    = Exchange,
    routing_key = RoutingKey},
  #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
  Publish = #'basic.publish'{exchange = Exchange, routing_key = RoutingKey},
  {ok, #queue_mgr_state{channel=Channel, publish=Publish}}.

handle_cast({send_to_queue, Payload}, State=#queue_mgr_state{channel=Channel, publish=Publish}) ->
  amqp_channel:cast(Channel, Publish , #amqp_msg{payload = Payload}),
  {noreply, State};

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

