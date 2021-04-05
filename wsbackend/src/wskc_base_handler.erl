-module(wskc_base_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
  {ok, Req, State}.

