%%%-------------------------------------------------------------------
%% @doc wskc public API
%% @end
%%%-------------------------------------------------------------------

-module(wskc_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [{"/", "base_handler", []}]}
				   ]),
  {ok, _} = cowboy:start_clear(
      my_http_listener, 
      [{port, 8080}],
      #{env => #{dispatch => Dispatch}}
  ).

start() -> kc_app:start([], []).

stop(_State) ->
  ok.

%% internal functions
