%%%-------------------------------------------------------------------
%% @doc wskc public API
%% @end
%%%-------------------------------------------------------------------

-module(wskc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", wskc_websocket_handler, []},
            {"/", cowboy_static, {file, "../frontend/dist/index.html"}},
            {"/[...]", cowboy_static, {dir, "../frontend/dist"}},
            {"/", wskc_base_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    wskc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
