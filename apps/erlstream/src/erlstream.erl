%% @author Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>
%% @copyright 2012 Anthonin Bonnefoy.

%% @doc erlstream startup code

-module(erlstream).
-author('Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the erlstream server.
start() ->
    [ensure_started(A) || A <- [crypto, public_key, inets, ssl]],
    application:start(erlstream).

%% @spec stop() -> ok
%% @doc Stop the erlstream server.
stop() ->
    Res = application:stop(erlstream),
    Res.

