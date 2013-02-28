%% @author Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>
%% @copyright 2012 Anthonin Bonnefoy.

%% @doc Supervisor for the erlstream application.

-module(erlstream_sup).
-author('Anthonin Bonnefoy <anthonin.bonnefoy@gmail.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_ARGS(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).


%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link(StartArgs) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  {ok, FileName} = application:get_env(conf),
  error_logger:info_msg("Supervisor starting with FileName: ~p~n", [FileName]),
  RestartStrategy    = one_for_all,
  MaxRestarts        = 0,
  MaxTimeBetRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
  ChildSpecs = [
    ?CHILD_ARGS(erlstream_worker, worker, [ FileName ])
  ],
  {ok,{SupFlags, ChildSpecs}}.

