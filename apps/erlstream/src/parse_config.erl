%% @author Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>
%% @copyright 2012 Anthonin Bonnefoy.
%% @doc Configuration parser.

-module(parse_config).
-author('Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>').

-include("erlstream.hrl").
-include("amqp_client.hrl").

-export([
  parse_stream_conf/1
  , parse_broker/1
  , parse_exchange/1
  , parse_queue_declare/1
  ]).

parse_stream_conf(FileName) ->
  {ok, Props} = file:consult(FileName),
  Key = proplists:get_value(url, Props),
  Secret = proplists:get_value(secret, Props),
  Url = proplists:get_value(url, Props),
  Token = proplists:get_value(token, Props),
  TokenSecret = proplists:get_value(tokenSecret, Props),
  Keywords = proplists:get_value(keywords, Props),
  Follow = proplists:get_value(follow, Props),
  #stream_configuration{
    key=Key , secret=Secret
    , token=Token, tokenSecret=TokenSecret
    , keywords=Keywords, url=Url
    , follow=Follow}.


parse_broker(FileName) ->
  {ok, Props} = file:consult(FileName),
  Host = proplists:get_value(host, Props),
  Port = proplists:get_value(port, Props),
  #amqp_params_network{
    host=Host
    , port=Port
  }.


parse_exchange(FileName) ->
  {ok, Props} = file:consult(FileName),
  Exchange = proplists:get_value(exchange, Props),
  Durable = proplists:get_value(durable, Props),
  #'exchange.declare'{
    exchange=Exchange,
    durable=Durable
  }.


parse_queue_declare(FileName) ->
  {ok, Props} = file:consult(FileName),
  Queue = proplists:get_value(queue, Props),
  #'queue.declare'{
    queue=Queue
  }.


