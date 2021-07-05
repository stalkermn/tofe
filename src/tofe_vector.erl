%%%-------------------------------------------------------------------
%%% @author Valerii Vasylkov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Providing a fun API for sliding a list backward and forward with adding empty spaces
%%% once values matched and combined
%%% @end
%%%-------------------------------------------------------------------
-module(tofe_vector).

-record(vector, {list = [] :: list(), values = [] :: list(), spaces = [] :: list()}).
%% API
-export([slide/2]).

%% --------------------------------------------------------------------
%% @doc
%% Performing sliding operation with returning new list of values including new spaces in case of
%% merged cells due to matched values in a list
%% @end
%% --------------------------------------------------------------------
slide(List, Direction) ->
    do_slide(List, Direction).

do_slide(List, forward) ->
    slide_list(List, last_to_first, front);

do_slide(List, backward) ->
    slide_list(List, first_to_last, rear).

slide_list(List, Direction, AddSpacesTo) ->
    [pipe](
        List,
        new(List),
        values_and_spaces(_),
        combine_matched(_, Direction),
        add_spaces(_, AddSpacesTo),
        list(_)
    ).

%% --------------------------------------------------------------------
%% @doc
%% Returning a list of values from a vector record
%% @end
%% --------------------------------------------------------------------
list(#vector{list = List}) -> List.
%% --------------------------------------------------------------------
%% @doc
%% Creating a vector record from initial list of values
%% @end
%% --------------------------------------------------------------------
new(List) -> #vector{list = List}.

%% --------------------------------------------------------------------
%% @doc
%% Updating vector record spaces and values before processing values
%% @end
%% --------------------------------------------------------------------
values_and_spaces(Vector)->
    V = lists:foldr(fun space_or_value/2, Vector, Vector#vector.list),
    V.
space_or_value(Item, Vector) when Item == null ->
    Vector#vector{spaces = [Item | Vector#vector.spaces]};
space_or_value(Item, Vector) ->
    Vector#vector{values = [Item | Vector#vector.values]}.

%% --------------------------------------------------------------------
%% @doc
%% Combining matched values and updating a vector record with a new list of values
%% @end
%% --------------------------------------------------------------------
combine_matched(#vector{values = Values} = Vector, first_to_last) ->
    [pipe](
        Values,
        combine_pairs(_),
        new_values(_, Vector)
    );
combine_matched(#vector{values = Values} = Vector, last_to_first) ->
    [pipe](
        Values,
        lists:reverse(_),
        combine_pairs(_),
        lists:reverse(_),
        new_values(_, Vector)
    ).

%% --------------------------------------------------------------------
%% @doc
%% Combining a list of values by matching same values.
%% @end
%% --------------------------------------------------------------------
combine_pairs([]) -> [];
combine_pairs([V1, V2 | Tail]) when V1 == V2 ->
    [V1 + V2 | combine_pairs(Tail)];
combine_pairs([V1 | Tail]) ->
    [V1 | combine_pairs(Tail)].

%% --------------------------------------------------------------------
%% @doc
%% Updating a list with new values in a vector record
%% @end
%% --------------------------------------------------------------------
new_values(NewValues, Vector) ->
    Vector#vector{
        list = NewValues,
        spaces = Vector#vector.spaces ++ missed_spaces(Vector#vector.values, NewValues)
    }.

%% --------------------------------------------------------------------
%% @doc
%% adding missed spaces appeared after merging values. Can be an empty list
%% @end
%% --------------------------------------------------------------------
missed_spaces(Values, NewValues) ->
    lists:duplicate(length(Values) - length(NewValues), null).

%% --------------------------------------------------------------------
%% @doc
%% adding spaces to a list of a vector record (to front or to back)
%% @end
%% --------------------------------------------------------------------
add_spaces(#vector{list = List} = Vector, front) ->
    Vector#vector{list = Vector#vector.spaces ++ List};
add_spaces(#vector{list = List} = Vector, rear) ->
    Vector#vector{list = List ++ Vector#vector.spaces}.