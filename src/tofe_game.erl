%%%-------------------------------------------------------------------
%%% @author Valerii Vasylkov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Providing game actor which will notify and handle all game activity here
%%% @end
%%%-------------------------------------------------------------------
-module(tofe_game).

-include("tofe.hrl").

-record(game, {
    name :: binary(),
    grid :: tofe_grid:grid(),
    players = #{} :: players(),
    chat_history =  queue:new() :: queue:queue()
}).
-opaque players() :: #{
    Username :: binary() => pid()
}.

-export_type([players/0]).

-behaviour(gen_server).
%% API for game communication
-export([cast/2]).

%% Gen server and supervisor callbacks.
-export([new_game/2]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(IDLE_TIMER_NO_CONNECTED_SECONDS, 30).
-define(SERVER, ?MODULE).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

new_game(Name, Size) ->
    start_link([Name, Size]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

cast(GamePid, CastMessage) ->
    ok = gen_server:cast(GamePid, CastMessage),
    {ok, sent}.


init([Name, Size]) ->
    erlang:send_after(timer:seconds(?IDLE_TIMER_NO_CONNECTED_SECONDS), self(), idle_no_players),
    Grid0 = tofe_grid:new(Size),
    Grid = tofe_grid:populate_randomly(Grid0),
    {ok, #game{name = Name, grid = Grid}}.

handle_call(_Request, _From, State = #game{}) ->
    {reply, ok, State}.

handle_cast({connected, Username, Pid}, State = #game{players = Players}) ->
    NewPlayers = Players#{Username => Pid},
    notify(connected, [Username], Players),
    notify(game, #{
        name => State#game.name,
        grid => State#game.grid,
        chat_history => [ [U, M] || {U, M} <- queue:to_list(State#game.chat_history)],
        players => maps:keys(NewPlayers)}, Pid),
    {noreply, State#game{players = NewPlayers}};

handle_cast({disconnected, Username}, State = #game{players = Players}) ->
    NewPlayers = maps:remove(Username, Players),

    NewState = State#game{players = NewPlayers},
    case has_no_players(NewPlayers) of
        true ->
            {stop, normal, NewState};
        false ->
            notify(disconnected, [Username], NewPlayers),
            {noreply, NewState}
    end;
handle_cast({move, Direction}, State) ->
    case move(Direction, State#game.grid) of
        {ok, NewGrid0} ->
            NewGrid = tofe_grid:populate_randomly(NewGrid0),
            notify(move, #{grid => NewGrid, direction => Direction}, State#game.players),
            {noreply, State#game{grid = NewGrid}};
        {game_over, NewGrid} ->
            notify(game_over, NewGrid, State#game.players),
            {stop, normal, State}
    end;

handle_cast({message, Username, Message}, State) ->
    NewState =
    case lists:member(Username, maps:keys(State#game.players)) of
        true ->
            NewState0 = State#game{chat_history = queue:in({Username, Message}, State#game.chat_history)},
            notify(message, #{Username =>  Message}, State#game.players),
            NewState0;
        false ->
            State
    end,
    {noreply, NewState};
handle_cast(stop, State = #game{}) ->
    {stop, normal, State};

handle_cast(_Request, State = #game{}) ->
    {noreply, State}.

handle_info(idle_no_players, State = #game{players = Players}) ->
    case has_no_players(Players) of
        true ->
            {stop, normal, State};
        false ->
            {noreply, State}
    end;
handle_info(_Info, State = #game{}) ->
    {noreply, State}.

terminate(_Reason, _State = #game{name = Name}) ->
    _ = tofe_game_sup:cleanup_game(Name, self()),
    ok.

code_change(_OldVsn, State = #game{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
has_no_players(NewPlayers) ->
    maps:size(NewPlayers) == 0.

notify(Event, Body, PlayerPid) when is_pid(PlayerPid) ->
    do_notify(Event, Body, PlayerPid);
notify(Event, Body, Players) ->
    [pipe](
        Players,
        maps:values(_),
        [do_notify(Event, Body, Pid) || Pid <- _]
    ).

do_notify(Event, Body, Pid) ->
    Pid ! #tofe_notification{event  = Event, body = Body}.

move(Direction, Grid) ->
    case tofe_grid:has_remaining_moves(Grid) of
        true ->
            {ok, do_a_move(Direction, Grid)};
        false ->
            {game_over, Grid}
    end.

do_a_move(up, Grid) ->
    [pipe](
        Grid,
        tofe_grid:slide_columns(_, backward),
        tofe_grid:populate_randomly(_)
    );
do_a_move(down, Grid) ->
    [pipe](
        Grid,
        tofe_grid:slide_columns(_, forward),
        tofe_grid:populate_randomly(_)
    );
do_a_move(left, Grid) ->
    [pipe](
        Grid,
        tofe_grid:slide_rows(_, backward),
        tofe_grid:populate_randomly(_)
    );
do_a_move(right, Grid) ->
    [pipe](
        Grid,
        tofe_grid:slide_rows(_, forward),
        tofe_grid:populate_randomly(_)
    ).