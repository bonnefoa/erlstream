%% @author Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>
%% @copyright 2012 Anthonin Bonnefoy.
%% @doc Lib for stream connection

-module(twitter_stream).
-author('Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>').

-include("erlstream.hrl").
-export([init_listen/2]).

loop(RequestId, Pid) ->
  receive
    {http, {RequestId, {{_, 401, _}, Headers, _}}} ->
      gen_server:cast(Pid, {unauthorized, Headers});
    {http, {RequestId, stream_start, Headers}} ->
      gen_server:cast(Pid, {stream_start, Headers}),
      loop(RequestId, Pid);
    {http, {RequestId, stream, Part}} ->
      case Part of
        <<"\r\n">> -> noop;
        _          -> gen_server:cast(Pid, {stream, Part})
      end,
      loop(RequestId, Pid);
    {http,{RequestId, stream_end, _Headers}} ->
      gen_server:cast(Pid, {stream_end});
    {http, {RequestId, {error, Reason}}} ->
      httpc:cancel_request(RequestId),
      gen_server:cast(Pid, {error, Reason});
    {stop} ->
      httpc:cancel_request(RequestId),
      gen_server:cast(Pid, {stop});
    Any->
      error_logger:info_msg("UNEXPECTED MESSAGE FROM TWITTER : ~p~n",[Any]),
      gen_server:cast(Pid, {error, Any})
      %% timeout
  after 60 * 1000 ->
      error_logger:info_msg("Got timeout ~n",[]),
      gen_server:cast(Pid, {error, timeout})
  end.
%% end of streaming data

clean_parameters([{_, ""} | X ]) ->
  clean_parameters(X);
clean_parameters([{K, V} | X ]) ->
  [ {K, V} | clean_parameters(X) ] ;
clean_parameters([]) ->
  [].

init_listen(Pid, #stream_configuration{
    key=Key, secret=Secret
    , token=Token, tokenSecret=TokenSecret
    , keywords=Keywords, url=Url
    , follow=Follow}) ->
  Consumer = {Key, Secret, hmac_sha1},
  ListKeywords = mochiweb_util:join(Keywords, ","),
  ListFollow = mochiweb_util:join(Follow, ","),
  ExtraParams = clean_parameters([{"track", ListKeywords }, {"follow", ListFollow} ]),
  SignedParams = oauth:sign("POST", Url, ExtraParams, Consumer, Token, TokenSecret),
  ContentType =  "application/x-www-form-urlencoded",
  Headers = [ {"Accept-Encoding", "deflate, gzip"} ],
  Request = {Url, Headers, ContentType, oauth:uri_params_encode(SignedParams) },
  HttpOptions = [ {connect_timeout, 5000} ],
  Options = [ {sync,false}, {stream, self} ],
  error_logger:info_msg("Request:~p~nOptions:~p~n", [Request, Options]),
  inets:start(),
  ssl:start(),
  case httpc:request(post, Request, HttpOptions, Options) of
    {ok, RequestId} ->
      loop(RequestId, Pid);
    {error, Reason} ->
      gen_server:cast(Pid, {error, Reason})
  end.

