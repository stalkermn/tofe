%%%-------------------------------------------------------------------
%%% @author Valerii Vasylkov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Game supervisor to handle all game registry here and keep an eye on game actors
%%% @end
%%%-------------------------------------------------------------------
-module(tofe_game_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_game/2, stop_game/1, list_games/0]).
-export([game_pid/1]).


-export([cleanup_game/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_game(binary(), non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_game(Name, Size) ->
    [maybe](Name,
        supervisor:start_child(?MODULE, [_, Size]),
        begin ets:insert(?MODULE, {Name, _}), {ok, _} end
    ).

-spec stop_game(binary())-> {ok, binary()} | {error, term()}.
stop_game(Name) ->
    [maybe](
        Name,
        game_pid(_),
        cleanup_game(Name, _),
        case supervisor:terminate_child(?MODULE, _) of
            ok -> {ok, Name};
            Err -> Err
        end
    ).

-spec cleanup_game(binary(), pid())-> {ok, pid()}.
cleanup_game(Name, Pid) ->
    ets:delete_object(?MODULE, {Name, Pid}),
    {ok, Pid}.

-spec game_pid(binary())-> {ok, pid()} | {error, not_found}.
game_pid(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Pid}] ->
            {ok, Pid};
        _ ->
            {error, not_found}
    end.

-spec list_games() -> [binary()].
list_games() ->
    {GameNames, _GamePids} = lists:unzip(ets:tab2list(?MODULE)),
    GameNames.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


%% @hidden
init([]) ->
    _ = ets:new(?MODULE, [named_table, public]),
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => simple_one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},

    GameServerChildSpec = #{id => 'tofe_game',
        start => {'tofe_game', new_game, []},
        restart => transient,
        shutdown => 2000,
        type => worker,
        modules => ['tofe_game']},

    {ok, {SupFlags, [GameServerChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
