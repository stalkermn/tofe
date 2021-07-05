%%%-------------------------------------------------------------------
%% @doc tofe public API
%% @end
%%%-------------------------------------------------------------------

-module(tofe_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", tofe_ws, []},
            {"/ws/:username", tofe_ws, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(tofe_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, _Pid} = tofe_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    _ = cowboy:stop_listener(tofe_listener),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
