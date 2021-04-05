-module(wskc_websocket_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
  {cowboy_websocket, Req, State}.
