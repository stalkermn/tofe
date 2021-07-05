%%%-------------------------------------------------------------------
%%% @author Valerii Vasylkov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Implementing all protocol stuff here including all validations and error handling
%%% @end
%%%-------------------------------------------------------------------
-module(tofe_protocol).


-define(DEFAULT_SIZE, 8).
-export([list_games/0]).
-export([new_game/1, new_game/2]).
-export([connected/2]).
-export([disconnected/2]).
-export([message/3]).
-export([move/2]).

-export([stop_game/1]).

-define(IS_ALLOWED_MOVE(Move), Move == <<"up">>;  Move == <<"down">>; Move ==  <<"left">>; Move == <<"right">>).

%% API
-spec list_games() -> [binary()].
list_games() ->
    tofe_game_sup:list_games().

-spec new_game(binary()) -> {ok, pid()}.
new_game(Name) ->
    new_game(Name, ?DEFAULT_SIZE).
new_game(Name, Size) when is_binary(Name), is_integer(Size), Size > 2 ->
    tofe_game_sup:start_game(Name, Size).

-spec stop_game(binary()) -> {ok, sent} | {error, term()}.
stop_game(Name) ->
    [maybe](Name,
        tofe_game_sup:game_pid(_),
        tofe_game:cast(_, stop)
    ).

-spec move(binary(), binary()) -> {ok, sent} | {error, term()}.
move(Name, Move) when ?IS_ALLOWED_MOVE(Move) ->

    [maybe](
        Name,
        tofe_game_sup:game_pid(Name),
        tofe_game:cast(_, {move, tofe_utils:to_atom(Move)})
    ).

message(Name, Username, Message) when is_binary(Name), is_binary(Username), is_binary(Message) ->
    [maybe](
        Name,
        tofe_game_sup:game_pid(Name),
        tofe_game:cast(_, {message, Username, Message})
    ).


-spec connected(binary(), binary()) -> {ok, sent} | {error, term()}.
connected(Name, Username) when is_binary(Name), is_binary(Username) ->
    [maybe](Name,
        tofe_game_sup:game_pid(_),
        tofe_game:cast(_, {connected, Username, self()})
    ).

-spec disconnected(binary(), binary()) -> {ok, sent} | {error, term()}.
disconnected(Name, Username) when is_binary(Name), is_binary(Username) ->
    [maybe](Name,
        tofe_game_sup:game_pid(_),
        tofe_game:cast(_, {disconnected, Username})
    ).
