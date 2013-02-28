%% @author Anthonin Bonnefoy anthonin.bonnefoy@gmail.com
%% @copyright 2012 Anthonin Bonnefoy.

%% @doc Callbacks for the erlstream application.

-module(erlstream_app).
-author('Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erlstream.
start(_StartType, _Args) ->
  erlstream_sup:start_link([]).

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erlstream.
stop(_State) ->
    ok.

