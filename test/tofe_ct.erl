-module(tofe_ct).

-include("tofe_ct.hrl").
%% API
-export([init_per_suite/1]).
-export([end_per_suite/1]).



init_per_suite(Config) ->
    application:ensure_all_started(tofe),
    application:ensure_all_started(gun),
    Config.

end_per_suite(_Config) ->
    application:stop(tofe),
    application:stop(gun),
    ok.
