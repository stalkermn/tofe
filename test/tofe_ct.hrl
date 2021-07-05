
-record(ws_client_ct, {username :: iolist(), conn_pid :: pid(), conn_ref :: reference()}).

-record(tofe_event_ct, {event :: atom(), payload :: maps:map()}).