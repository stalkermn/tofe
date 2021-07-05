-module(tofe_game_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("tofe.hrl").

%% CT
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test Cases
-export([start_stop_game/1]).
-export([connect_disconnect/1]).
-export([idle_no_players/1]).
-export([game_moves/1]).
-export([chat_in_game/1]).
%%%===================================================================
%%% CT Callbacks
%%%===================================================================

-spec all() -> [atom()].
all() ->
    [
        start_stop_game,
        connect_disconnect,
        game_moves,
        chat_in_game,
        idle_no_players
    ].

init_per_suite(Config) ->
    tofe_ct:init_per_suite(Config).

end_per_suite(Config) ->
    tofe_ct:end_per_suite(Config).


-spec init_per_testcase(atom(), term()) -> term().
init_per_testcase(chat_in_game, Config0) ->
    init_per_testcase(connect_users, Config0);

init_per_testcase(game_moves, Config0) ->
    init_per_testcase(connect_users, Config0);

init_per_testcase(connect_users, Config0) ->
    Config = init_per_testcase(generic, Config0),
    GameName = proplists:get_value(game_name, Config),
%%    GamePid = proplists:get_value(game_pid, Config),
    Username1 = <<"testusername1">>,
    Username2 = <<"testusername2">>,
    {ok, sent} = tofe_protocol:connected(GameName, Username1),
    {ok, sent} = tofe_protocol:connected(GameName, Username2),
    timer:sleep(50),
    shell_default:flush(),

    [{user1, Username1}, {user2, Username2} | Config];

init_per_testcase(_Case, Config) ->
%%    application:ensure_all_started(tofe),
    GameName = <<"testnewgamename">>,
    {ok, GamePid} = tofe_protocol:new_game(GameName),
    [{game_name, GameName}, {game_pid, GamePid} | Config].

-spec end_per_testcase(atom(), pwt_ct:config()) -> ok.
end_per_testcase(_Case, Config) ->
    GameName = proplists:get_value(game_name, Config),
    catch tofe_protocol:stop_game(GameName),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

start_stop_game(Config) ->
    GameName = proplists:get_value(game_name, Config),
    GamePid = proplists:get_value(game_pid, Config),

    ?assert(lists:member(GameName, tofe_protocol:list_games())),
    ?assert(is_process_alive(GamePid)),
    {ok, sent} = tofe_protocol:stop_game(GameName),
    timer:sleep(100), % sleep while process is terminating
    false = is_process_alive(GamePid),
    [] = tofe_protocol:list_games().

%% @doc this test is showing that in case of idle timer will trigger to stop the game if there are
%% no connected users initially after 30 seconds
%% @end
idle_no_players(Config) ->
%%    GameName = proplists:get_value(game_name, Config),
    GamePid = proplists:get_value(game_pid, Config),

    timer:sleep(timer:seconds(31)),
    false = is_process_alive(GamePid),
    [] = tofe_protocol:list_games().


%% @doc
%% in this test case we are going to check all events associated to connect/disconnect of users. ensuring that they are
%% going to be connected to the same game and receiving all necessary details about game, etc.
%% @end
connect_disconnect(Config) ->
    GameName = proplists:get_value(game_name, Config),
    GamePid = proplists:get_value(game_pid, Config),

%%  connecting to a game
    Username1 = <<"testusername1">>,
    Username2 = <<"testusername2">>,
    {ok, sent} = tofe_protocol:connected(GameName, Username1),
    timer:sleep(50),
%%    connected/disconnected events might be received only for those who are in game. So there are no users in game initially,
%%    no connected notifications are expected
    no_such_event = get_an_event(connected),
    true = is_process_alive(GamePid),
%%    for those who is just connected, sending game details as a separated notification event=game
    #tofe_notification{ body = #{grid := GameGrid1} } = get_an_event(game),

%%  Connecting second player
    {ok, sent} = tofe_protocol:connected(GameName, Username2),
    timer:sleep(50),
%%  username1 receiving that another player has been connected to a game
    #tofe_notification{event = connected, body = [Username2]} = get_an_event(connected),
%%  username2 who is just connected, sending game details as a separated notification event=game
    #tofe_notification{ body = #{grid := GameGrid2, chat_history := [], players := MightBeTwoPlayers} } = get_an_event(game),
    2 = erlang:length(MightBeTwoPlayers),
%%    ensure that the grid is the same which means they both are connected to the same game :)
    ?assertEqual(GameGrid1, GameGrid2),

%%    username 1 going to disconnected
    tofe_protocol:disconnected(GameName, Username1),
    timer:sleep(50),

%%  Players who still in game receiving notification about disconnected `username1`
    #tofe_notification{body = [Username1]} = get_an_event(disconnected), % `username2` received a notification
    no_such_event = get_an_event(disconnected), % `username1` might not receive this

%% username2 disconnected. There are no more users is in game
    tofe_protocol:disconnected(GameName, Username2),
    timer:sleep(50),

%% Assuming that no users are in game we do stop a game
    false = is_process_alive(GamePid),
    [] = tofe_protocol:list_games().


chat_in_game(Config) ->
    GameName = proplists:get_value(game_name, Config),
    Username1 = proplists:get_value(user1, Config),
    Username2 = proplists:get_value(user2, Config),
    Message1 = <<"this is a test message from ", Username1/binary>>,
    Message2 = <<"this is a test message from ", Username2/binary>>,

    Username3 = <<"testusername3">>,

    {ok, sent} = tofe_protocol:message(GameName, Username1, Message1),
    timer:sleep(50),
%%    ensure that both of users got a message event
    #tofe_notification{event = message, body =#{Username1 := Message1} } = get_an_event(message),
    #tofe_notification{event = message, body =#{Username1 := Message1} } = get_an_event(message),
%%    check that there are no more events
    no_such_event = get_an_event(messages),

    {ok, sent} = tofe_protocol:message(GameName, Username2, Message2),
    timer:sleep(100),

    %%    ensure that both of users got a message event
    #tofe_notification{event = message, body =#{Username2 := Message2} } = get_an_event(message),
    #tofe_notification{event = message, body =#{Username2 := Message2} } = get_an_event(message),
%%    check that there are no more events
    no_such_event = get_an_event(messages),

    {ok, sent} = tofe_protocol:connected(GameName, Username3),
    timer:sleep(50),
    #tofe_notification{ body = #{chat_history := [[Username1, Message1],[Username2,  Message2]]} } = get_an_event(game),
    #tofe_notification{event = connected, body = [Username3]} = get_an_event(connected),
    #tofe_notification{event = connected, body = [Username3]} = get_an_event(connected),
    ok.




game_moves(Config) ->
    GameName = proplists:get_value(game_name, Config),

%%  connecting to a game by both of users
    Username1 = <<"testusername1">>,
    Username2 = <<"testusername2">>,
    {ok, sent} = tofe_protocol:connected(GameName, Username1),
    timer:sleep(50),
    #tofe_notification{event = game, body = #{grid := Grid0}} = get_an_event(game),
    {ok, sent} = tofe_protocol:connected(GameName, Username2),
    timer:sleep(50),
    #tofe_notification{event = game, body = #{grid := Grid0}} = get_an_event(game),
%%    cleanup remaining events from mailbox
    shell_default:flush(),


%%    Performing a move. Comparing that grid changed and both of usernames got an event
    {ok, sent} = tofe_protocol:move(GameName, <<"up">>),
    timer:sleep(200),
    #tofe_notification{event = move, body = NewGrid1} = get_an_event(move),
    #tofe_notification{event = move, body = NewGrid1} = get_an_event(move),
    ?assertNotEqual(Grid0, NewGrid1),

%%    Performing a move. Comparing that grid changed from initinal, and after first move and both of usernames got an event
    {ok, sent} = tofe_protocol:move(GameName, <<"down">>),
    timer:sleep(50),
    #tofe_notification{event = move, body = NewGrid2} = get_an_event(move),
    #tofe_notification{event = move, body = NewGrid2} = get_an_event(move),
    ?assertNotEqual(Grid0, NewGrid2),
    ?assertNotEqual(NewGrid1, NewGrid2),

%%    Performing a move. Comparing that grid changed from initinal, and after first and second moves and both of usernames got an event
    {ok, sent} = tofe_protocol:move(GameName, <<"right">>),
    timer:sleep(50),
    #tofe_notification{event = move, body = NewGrid3} = get_an_event(move),
    #tofe_notification{event = move, body = NewGrid3} = get_an_event(move),
    ?assertNotEqual(Grid0, NewGrid3),
    ?assertNotEqual(NewGrid1, NewGrid3),
    ?assertNotEqual(NewGrid2, NewGrid3),

%%    Performing a move. Comparing that grid changed from initinal, and after first and second and third moves and both of usernames got an event
    {ok, sent} = tofe_protocol:move(GameName, <<"left">>),
    timer:sleep(50),
    #tofe_notification{event = move, body = NewGrid4} = get_an_event(move),
    #tofe_notification{event = move, body = NewGrid4} = get_an_event(move),
    ?assertNotEqual(Grid0, NewGrid4),
    ?assertNotEqual(NewGrid1, NewGrid4),
    ?assertNotEqual(NewGrid2, NewGrid4),
    ?assertNotEqual(NewGrid3, NewGrid4),

    ok.


%% @doc
%% util fun to get a notification by event name, otherwise - no_such_event
%% @end
get_an_event(Event) ->
    receive
        #tofe_notification{event = Event} = Notification ->
            Notification
    after
        0 ->
            no_such_event
    end.