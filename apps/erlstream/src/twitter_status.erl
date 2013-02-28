%% @author Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>
%% @copyright 2012 Anthonin Bonnefoy.
%% @doc Lib for status connection

-module(twitter_status).
-author('Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>').

-include("erlstream.hrl").
-export([request_status/2]).

request_status(#status_request{key=Key, secret=Secret , token=Token, tokenSecret=TokenSecret}, TweetId) ->
    inets:start(),
    ssl:start(),

    Url = "http://api.twitter.com/1/statuses/show.json",
    Consumer = {Key, Secret, hmac_sha1},
    ExtraParams = [ {"id", TweetId} ],

    SignedParams = oauth:sign("GET", Url, [], Consumer, Token, TokenSecret),
    Request = {Url, [], [], oauth:uri_params_encode(SignedParams) },
    error_logger:info_msg("Request:~p~n", [Request]),
    case httpc:request(get, Request, [], []) of
      {ok, Result} ->
        error_logger:info_msg("Result:~p~n", [Result]);
      {error, Reason} ->
        error_logger:error_msg("error:~p~n", [Reason])
    end.

