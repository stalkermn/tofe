%%%-------------------------------------------------------------------
%%% @author Valerii Vasylkov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Providing a game grid functionality for a 2048 game with a desired size
%%% @end
%%%-------------------------------------------------------------------
-module(tofe_grid).

-define(DEFAULT_POPULATE_TIMES, 4).

%% API


-export([new/1]).
-export([populate_randomly/1, populate_randomly/2]).
-export([has_remaining_moves/1]).
-export([slide_rows/2]).
-export([slide_columns/2]).

-export([has_empty_spaces/1, has_possible_moves/1]).

-opaque grid() :: [list()].

-opaque row() :: list().

-export_type [grid/0, row/0].


-spec new(Size :: non_neg_integer()) -> grid().
%% --------------------------------------------------------------------
%% @doc
%% Performing creating a new grid (matrix) by desired size
%% @end
%% --------------------------------------------------------------------
new(Size) ->
    [ [null || _ColI <- lists:seq(1, Size)] || _RowI <-lists:seq(1, Size)].

-spec has_remaining_moves(grid()) -> boolean().
%% --------------------------------------------------------------------
%% @doc
%% Performing check if there are any available moves left before end the game.
%% @end
%% --------------------------------------------------------------------
has_remaining_moves(Grid) ->
    has_empty_spaces(Grid) andalso has_possible_moves(Grid).

%% --------------------------------------------------------------------
%% @doc
%% Randomly populating grid with values (2 or 4). By default - 4 times
%% @end
%% --------------------------------------------------------------------
-spec populate_randomly(grid()) -> grid().
populate_randomly(Grid) ->
    populate_randomly(Grid, ?DEFAULT_POPULATE_TIMES).

-spec populate_randomly(grid(), Repeat :: non_neg_integer()) -> grid().
%% --------------------------------------------------------------------
%% @doc
%% Randomly populating grid with values (2 or 4). Can specify a number of items added to the grid
%% @end
%% --------------------------------------------------------------------
populate_randomly(Grid, 0) ->
    Grid;
populate_randomly(Grid, Repeat) ->
    lists:foldl(
        fun(_RepeatI, GridI) ->
            random_put(GridI, get_a_number())
        end, Grid, lists:seq(1, Repeat)).

%% --------------------------------------------------------------------
%% @doc
%% Randomly getting a number 2 or 4 in a 75% proportion of 2 instead of 2
%% @end
%% --------------------------------------------------------------------
get_a_number() ->
    tofe_utils:pick_random([2,2,2,4]).

%% --------------------------------------------------------------------
%% @doc
%% Randomly put a value into a cell which has no value
%% @end
%% --------------------------------------------------------------------
random_put(Grid, Value) ->
    [pipe](
        empty_cells(Grid),
        tofe_utils:pick_random(_),
        put_at(Grid, _, Value)).

%% --------------------------------------------------------------------
%% @doc
%% Getting a list of empty cells :: Returning [{X, Y} | Tail}] when X - row number, Y - Col number
%% @end
%% --------------------------------------------------------------------
empty_cells(Grid) ->
    lists:flatten([
        [ {RowI, ColI}
            || {ColI, null} <- tofe_utils:with_index(Row)]
        || {RowI, Row} <- tofe_utils:with_index(Grid) ]).

%% --------------------------------------------------------------------
%% @doc
%% Put a value into the X row, Y col
%% @end
%% --------------------------------------------------------------------
put_at(Grid, {X, Y}, Value) ->
    GridTuple = list_to_tuple(Grid),
    [pipe](GridTuple,
        erlang:element(X, _),
        list_to_tuple(_),
        erlang:setelement(Y, _, Value),
        tuple_to_list(_),
        erlang:setelement(X, GridTuple, _),
        tuple_to_list(_)
    ).

%% --------------------------------------------------------------------
%% @doc
%% Sliding rows
%% @end
%% --------------------------------------------------------------------
slide_rows(Grid, Direction) ->
    slide_vectors(Grid, Direction).

%% --------------------------------------------------------------------
%% @doc
%% Sliding a column with a transpose approach
%% @end
%% --------------------------------------------------------------------
slide_columns(Grid, Direction) ->
    [pipe](
        Grid,
        tofe_utils:transpose(_),
        slide_vectors(_, Direction),
        tofe_utils:transpose(_)
    ).

slide_vectors(VectorLists, Direction) ->
    [ slide_vector(VectorList, Direction) || VectorList <- VectorLists].

slide_vector(VectorList, Direction) ->
    tofe_vector:slide(VectorList, Direction).

%% --------------------------------------------------------------------
%% @doc
%% Lazy checking for empty spaces in a grid, in case of first side is executed, second expr will not be evaluated
%% @end
%% --------------------------------------------------------------------

has_empty_spaces([]) -> false;
has_empty_spaces([null | _Row]) -> true;
has_empty_spaces([I | Row]) when is_integer(I) -> has_empty_spaces(Row);
has_empty_spaces([Row | Rows]) when is_list(Row) ->
    case has_empty_spaces(Row) of
        false -> has_empty_spaces(Rows);
        true -> true
    end.

%% --------------------------------------------------------------------
%% @doc
%% Lazy checking for possible moves. First evaluated expr which returns true stops the pipeline of evaluating others.
%% @end
%% --------------------------------------------------------------------
has_possible_moves(Grid) ->
    slide_rows(Grid, forward) /= Grid orelse
    slide_rows(Grid, backward) /= Grid orelse
    slide_columns(Grid, forward) /= Grid orelse
    slide_columns(Grid, backward) /= Grid.
