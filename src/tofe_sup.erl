%%%-------------------------------------------------------------------
%%% @author Valerii Vasylkov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Top level supervisor. Starting a child supervisor responsible for game management
%%% @end
%%%-------------------------------------------------------------------

-module(tofe_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @hidden
init([]) ->
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},

    GameSupChildSpec = #{id => 'tofe_game_sup',
        start => {'tofe_game_sup', start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => supervisor,
        modules => ['tofe_game_sup']},

    {ok, {SupFlags, [GameSupChildSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
