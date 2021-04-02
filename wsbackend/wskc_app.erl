%%%-------------------------------------------------------------------
%% @doc wskc public API
%% @end
%%%-------------------------------------------------------------------

-module(wskc_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start(_StartType, _StartArgs) ->
  

start() -> kc_app:start([], []).

stop(_State) ->
  ok.

%% internal functions
