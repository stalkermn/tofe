%%%-------------------------------------------------------------------
%%% @author Valerii Vasylkov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Util functions for working with lists
%%% @end
%%%-------------------------------------------------------------------
-module(tofe_utils).

%% API
-export([transpose/1]).
-export([with_index/1]).
-export([pick_random/1]).
%% Type casting functions
-export([to_atom/1]).
-export([rand/1]).

-spec transpose([list()]) -> [list()].
transpose([[]|_]) -> [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

-spec with_index(List :: list()) -> proplists:proplist().
with_index(List) ->
    lists:zip(lists:seq(1, length(List)), List).

-spec pick_random(list()) -> term().
pick_random(List) ->
    Index = erlang:phash(os:timestamp(), length(List)),
    lists:nth(Index, List).

to_atom(V) when is_binary(V) ->
    binary_to_existing_atom(V).

rand(Length) ->
    bin2hex(crypto:strong_rand_bytes(Length)).

bin2hex(Bytes) ->
    erlang:iolist_to_binary([begin if N < 10 -> 48 + N; true -> 87 + N end end || <<N:4>> <= Bytes]).
