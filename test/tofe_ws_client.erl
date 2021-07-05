-module(tofe_ws_client).
-include("tofe_ct.hrl").

%% API
-export([]).


-export([ws_client/1]).
-export([ws_stop/1]).
-export([ws_receive/1]).
-export([ws_receive_event/2]).
-export([ws_send/2, ws_send/3]).


ws_client(Username)->
    [maybe](Username,
        application:get_env(tofe, http_port),
        gun:open("localhost", _),
        begin {ok, gun:ws_upgrade(_,  ["/ws/", Username])} end,
        ws_connected(Username, _)
    ).

ws_connected(Username, StreamRef) ->
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _} ->
            {ok, #ws_client_ct{username = Username, conn_ref = StreamRef, conn_pid = ConnPid}}
    after 5000 ->
        {error, timeout}
    end.

ws_stop(#ws_client_ct{conn_pid = ConnPid}) ->
    catch gun:shutdown(ConnPid).



ws_receive(#ws_client_ct{conn_pid = ConnPid}) ->
    receive
        {gun_ws, ConnPid, _StreamRef, {_, Frame}} ->
            jiffy:decode(Frame, [return_maps])
    after
        2000 ->
            no_frames
    end.

ws_receive_event(#ws_client_ct{} = Cli, EventA) ->
    Event = atom_to_binary(EventA),
    case ws_receive(Cli) of
        #{<<"event">> := Event, <<"payload">> := Payload} ->
            #tofe_event_ct{event = EventA, payload = Payload};
        OtherEvent ->
            ct:pal("Event: ~p",[OtherEvent]),
            not_such_event
    end.


ws_send(#ws_client_ct{} = Cli, Action) ->
    ws_send(Cli, Action, #{}).

ws_send(Cli, Action, Payload) ->
    gun:ws_send(Cli#ws_client_ct.conn_pid,
        Cli#ws_client_ct.conn_ref,
        {text, jiffy:encode(#{action => atom_to_binary(Action), payload => Payload})}).