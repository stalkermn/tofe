%%%-------------------------------------------------------------------
%%% @author Valerii Vasylkov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% WS interface for game actor interaction. all events are async, event driven arch
%%% @end
%%%-------------------------------------------------------------------
-module(tofe_ws).

-include("tofe.hrl").

-behavior(cowboy_websocket).
%% API
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-export([terminate/3]).

-record(ws_state, {username :: binary(), game :: binary() | undefined}).


init(Req, _State0) ->
    Username = cowboy_req:binding(username, Req, <<"player", (tofe_utils:rand(12))/binary>>),
    {cowboy_websocket, Req, #ws_state{username =  Username}}.

websocket_init(State) ->
    {ok, State, hibernate}.

websocket_handle({ping,_}, State) ->
    {reply, pong, State, hibernate};
websocket_handle({pong,_}, State) ->
    {reply, pong, State, hibernate};

websocket_handle({text, <<"pong">>}, State) ->
    {ok, State, hibernate};
websocket_handle({binary, Payload}, State) ->
    try
        NewState = handle_payload(jiffy:decode(Payload, [return_maps]), State),
        {ok, NewState, hibernate}
    catch
        _Ex:Err:Stack ->
            ErrPayload = iolist_to_binary(io_lib:format("~p", [Err])),
            StackTrace = iolist_to_binary(io_lib:format("~p", [Stack])),
            {reply, {text, jiffy:encode(#{event => error, reason => ErrPayload, stack => StackTrace})}, State}
    end;

websocket_handle({text, Payload}, State) ->
    websocket_handle({binary, Payload}, State);
websocket_handle(_Frame, State) ->
    {ok, State}.

terminate(_Reason, _Req, State) ->
    case State#ws_state.game of
        undefined ->
            ok;
        Game ->
            _ = tofe_protocol:disconnected(Game, State#ws_state.username),
            ok
    end.

websocket_info(#tofe_notification{event = Event, body = Body}, State) ->
    {reply, {text, jiffy:encode(#{event => Event, payload => Body})}, State}.

handle_payload(#{<<"action">> := <<"join_game">>, <<"payload">> := Payload}, State) ->
    Name = game_name(Payload),
    _ = tofe_protocol:connected(Name, State#ws_state.username),
    State#ws_state{game = Name};
handle_payload(#{<<"action">> := <<"new_game">>} = Body, State) ->
    Name = game_name(maps:get(<<"payload">>, Body, #{})),
    _ = tofe_protocol:new_game(Name),
    _ = tofe_protocol:connected(Name, State#ws_state.username),
    State#ws_state{game = Name};
handle_payload(#{<<"action">> := <<"move">>, <<"payload">> := Payload}, State) ->
    Move = move(Payload),
    _ = tofe_protocol:move(State#ws_state.game, Move),
    State;
handle_payload(#{<<"action">> := <<"message">>, <<"payload">> := Payload}, State) ->
    Message = message(Payload),
    _ = tofe_protocol:message(State#ws_state.game, State#ws_state.username, Message),
    State;
handle_payload(#{<<"action">> := <<"list_games">>}, State) ->
    ListGames = tofe_protocol:list_games(),
    self() ! #tofe_notification{event = list_games, body = ListGames},
    State.

game_name(Payload) ->
    maps:get(<<"name">>, Payload, <<"2048game-", (tofe_utils:rand(10))/binary>>).

move(Payload) ->
    maps:get(<<"move">>, Payload).

message(Payload) ->
    maps:get(<<"message">>, Payload).