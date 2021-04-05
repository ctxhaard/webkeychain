-module(wskc_base_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
  {"/assets/[...]", cowboy_static, {priv_dir, my_app, "static/assets"}}.

