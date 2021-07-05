-module(tofe_ws_client_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("tofe.hrl").
-include("tofe_ct.hrl").


%% CT
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Export test cases
-export([list_games/1]).
-export([basic_gameplay/1]).
-export([chat/1]).


-spec all() -> [atom()].
all() ->
    [
        list_games,
        basic_gameplay,
        chat
    ].

init_per_suite(Config) ->
    tofe_ct:init_per_suite(Config).

end_per_suite(Config) ->
    tofe_ct:end_per_suite(Config).


-spec init_per_testcase(atom(), term()) -> term().
init_per_testcase(_Case, Config) ->
    _ = shell_default:flush(),
    Username1 = <<"testusername1">>,
    Username2 = <<"testusername2">>,
    [{user1, element(2, tofe_ws_client:ws_client(Username1))},
        {user2, element(2, tofe_ws_client:ws_client(Username2))} | Config].

-spec end_per_testcase(atom(), pwt_ct:config()) -> ok.
end_per_testcase(_Case, Config) ->
    [ tofe_ws_client:ws_stop(V)  ||  {K, V} <- Config, lists:member(K, [user1, user2])],
    _ = shell_default:flush(),
    ok.



list_games(Config) ->
    User1Ws = #ws_client_ct{} = proplists:get_value(user1, Config),
    _ = tofe_ws_client:ws_send(User1Ws, list_games),
    #tofe_event_ct{event = list_games, payload = []} = tofe_ws_client:ws_receive_event(User1Ws, list_games),
    _ = tofe_ws_client:ws_send(User1Ws, new_game),
    _ = tofe_ws_client:ws_send(User1Ws, list_games),
    #tofe_event_ct{event = _, payload = ListGamesPayload} = tofe_ws_client:ws_receive_event(User1Ws, list_games),
    1 = erlang:length(ListGamesPayload),
    [tofe_protocol:stop_game(GName) || GName <- ListGamesPayload].

basic_gameplay(Config) ->
    User1Ws = #ws_client_ct{username = Username1} = proplists:get_value(user1, Config),
    User2Ws = #ws_client_ct{username = Username2} = proplists:get_value(user2, Config),

%%    creating a new game by user1. User1 will autojoin to game and will receive a game state
    _ = tofe_ws_client:ws_send(User1Ws, new_game),

%%  receiving init game state info for user1
    #tofe_event_ct{event = game,
        payload = #{<<"grid">> := Grid0, <<"players">> := [Username1], <<"name">> := GameName}
    } = tofe_ws_client:ws_receive_event(User1Ws, game),

%%  Listing games available by user2 and trying to join
    _ = tofe_ws_client:ws_send(User2Ws, list_games),
    #tofe_event_ct{event = _, payload = [GameName]} = tofe_ws_client:ws_receive_event(User2Ws, list_games),

    _ = tofe_ws_client:ws_send(User2Ws, join_game, #{name => GameName}),
    #tofe_event_ct{event = game,
        payload = #{<<"grid">> := Grid0, <<"players">> := [Username1, Username2], <<"name">> := GameName}
    } = tofe_ws_client:ws_receive_event(User2Ws, game),

%% User1 will receive `connected` event noticing that user2 has been connected
    #tofe_event_ct{event = connected, payload = [Username2]} = tofe_ws_client:ws_receive_event(User1Ws, connected),

%%    Let's try to pass some moves

    _ = tofe_ws_client:ws_send(User1Ws, move, #{move => <<"up">>}),
    #tofe_event_ct{event = move,
        payload = #{<<"grid">> := GridUpMove}
    } = tofe_ws_client:ws_receive_event(User1Ws, move),
    #tofe_event_ct{event = move,
        payload = #{<<"grid">> := GridUpMove}
    } = tofe_ws_client:ws_receive_event(User2Ws, move),
    ?assert(GridUpMove /= Grid0),

    _ = tofe_ws_client:ws_send(User2Ws, move, #{move => <<"down">>}),
    #tofe_event_ct{event = move,
        payload = #{<<"grid">> := GridUpDownMove}
    } = tofe_ws_client:ws_receive_event(User1Ws, move),
    #tofe_event_ct{event = move,
        payload = #{<<"grid">> := GridUpDownMove}
    } = tofe_ws_client:ws_receive_event(User2Ws, move),
    ?assert(GridUpDownMove /= Grid0),
    ?assert(GridUpDownMove /= GridUpMove),

    _ = tofe_ws_client:ws_send(User1Ws, move, #{move => <<"right">>}),
    #tofe_event_ct{event = move,
        payload = #{<<"grid">> := GridUpDownRightMove}
    } = tofe_ws_client:ws_receive_event(User1Ws, move),
    #tofe_event_ct{event = move,
        payload = #{<<"grid">> := GridUpDownRightMove}
    } = tofe_ws_client:ws_receive_event(User2Ws, move),
    ?assert(GridUpDownRightMove /= Grid0),
    ?assert(GridUpDownRightMove /= GridUpMove),
    ?assert(GridUpDownRightMove /= GridUpDownMove),

    _ = tofe_ws_client:ws_send(User1Ws, move, #{move => <<"left">>}),
    #tofe_event_ct{event = move,
        payload = #{<<"grid">> := GridUpDownRightLeftMove}
    } = tofe_ws_client:ws_receive_event(User1Ws, move),
    #tofe_event_ct{event = move,
        payload = #{<<"grid">> := GridUpDownRightLeftMove}
    } = tofe_ws_client:ws_receive_event(User2Ws, move),

    ?assert(GridUpDownRightLeftMove /= Grid0),
    ?assert(GridUpDownRightLeftMove /= GridUpMove),
    ?assert(GridUpDownRightLeftMove /= GridUpDownMove),
    ?assert(GridUpDownRightLeftMove /= GridUpDownRightMove),

    _ = tofe_ws_client:ws_stop(User1Ws),

    #tofe_event_ct{event = disconnected, payload = [Username1]} = tofe_ws_client:ws_receive_event(User2Ws, disconnected),

    _ = tofe_ws_client:ws_send(User2Ws, list_games),
    #tofe_event_ct{event = _, payload = [GameName]} = tofe_ws_client:ws_receive_event(User2Ws, list_games),

    _ = tofe_ws_client:ws_stop(User2Ws),
    timer:sleep(100),
    tofe_game_sup:list_games() == 0.


chat(Config) ->
    User1Ws = #ws_client_ct{username = Username1} = proplists:get_value(user1, Config),
    User2Ws = #ws_client_ct{username = Username2} = proplists:get_value(user2, Config),

    Message1 = <<"this is a test message from", Username1/binary>>,
    Message2 = <<"this is a test message from", Username2/binary>>,

    %%    creating a new game by user1. User1 will autojoin to game and will receive a game state
    _ = tofe_ws_client:ws_send(User1Ws, new_game),
    #tofe_event_ct{event = game,
        payload = #{<<"players">> := [Username1], <<"name">> := GameName}
    } = tofe_ws_client:ws_receive_event(User1Ws, game),

    %% user1 is sending a message to a game context and get's notified about new message from game process
    _ = tofe_ws_client:ws_send(User1Ws, message, #{message => Message1}),
    #tofe_event_ct{event = message, payload = #{Username1 := Message1}} = tofe_ws_client:ws_receive_event(User1Ws, message),

    %% user2 is join to the game and see the chat history of the game
    _ = tofe_ws_client:ws_send(User2Ws, join_game, #{name => GameName}),
    #tofe_event_ct{event = game,
        payload = #{<<"players">> := [Username1, Username2], <<"name">> := GameName, <<"chat_history">> := [[Username1, Message1]]}
    } = tofe_ws_client:ws_receive_event(User2Ws, game),

    %% user1 is sending  a message where 2 players already in the game. Both of them receiving message event from the server

    shell_default:flush(),
    _ = tofe_ws_client:ws_send(User1Ws, message, #{message => <<"another message">>}),
    #tofe_event_ct{event = message, payload = #{Username1 := <<"another message">>}} = tofe_ws_client:ws_receive_event(User2Ws, message),
    #tofe_event_ct{event = message, payload = #{Username1 := <<"another message">>}} = tofe_ws_client:ws_receive_event(User1Ws, message),

    %% user2 is able to send a message and both of players receiving those
    _ = tofe_ws_client:ws_send(User2Ws, message, #{message => Message2}),
    #tofe_event_ct{event = message, payload = #{Username2 := Message2}} = tofe_ws_client:ws_receive_event(User1Ws, message),
    #tofe_event_ct{event = message, payload = #{Username2 := Message2}} = tofe_ws_client:ws_receive_event(User2Ws, message),

%%  disconnecting both of the clients
    _ = tofe_ws_client:ws_stop(User1Ws),
    _ = tofe_ws_client:ws_stop(User2Ws),
    timer:sleep(100),
%%    once no more players in game, game is going to be stopped
    [] = tofe_game_sup:list_games().

