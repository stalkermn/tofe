-module(tofe_grid_test).

-include_lib("eunit/include/eunit.hrl").


%% 2x2 grid test
'2x2_grid_test'() ->
    ?assertEqual([[null,null], [null,null]], tofe_grid:new(2) ).

%% 5x5 grid test
'5x5_grid_test'() ->
    ?assertEqual([
        [null,null,null,null,null],
        [null,null,null,null,null],
        [null,null,null,null,null],
        [null,null,null,null,null],
        [null,null,null,null,null]
    ],
        tofe_grid:new(5) ).

%% grid pop test
grid_population_2_test()->
    ExpPopulatedItems = 2,
    Grid0 = tofe_grid:new(4),
    Grid = tofe_grid:populate_randomly(Grid0, ExpPopulatedItems),
    ItemsList = [ Item ||  Item<- lists:flatten(Grid), Item /= null],
    ?assertEqual(ExpPopulatedItems, erlang:length(ItemsList)).

grid_population_4_test()->
    ExpPopulatedItems = 4,
    Grid0 = tofe_grid:new(4),
    Grid = tofe_grid:populate_randomly(Grid0, ExpPopulatedItems),
    ItemsList = [ Item ||  Item<- lists:flatten(Grid), Item /= null],
    ?assertEqual(ExpPopulatedItems, erlang:length(ItemsList)).

grid_population_0_test()->
    ExpPopulatedItems = 0,
    Grid0 = tofe_grid:new(4),
    Grid = tofe_grid:populate_randomly(Grid0, ExpPopulatedItems),
    ItemsList = [ Item ||  Item<- lists:flatten(Grid), Item /= null],
    ?assertEqual(ExpPopulatedItems, erlang:length(ItemsList)),
    ?assertEqual(Grid0, Grid).

-define(grid1_available,  [
    [2  , null, null, 16 ],
    [null, null, 2  , 4  ],
    [2  , null, 2  , 2  ],
    [2  , null, 2  , 4  ]
]).
-define(grid2_available, [
    [2048, 64 , 1024, 32  ],
    [2048, 2  , 16  , 32  ],
    [128 , null, 2   , null ],
    [2   , 512, 2   , 2   ]
] ).
-define(grid_3_game_over,  [
    [ 2 , 4, 8, 16],
    [ 16, 8, 4, 2 ],
    [ 2 , 4, 8, 16],
    [ 16, 8, 4, 2 ]
]).


grid_rows_fwd_1_test() ->
    Grid1 = tofe_grid:slide_rows(?grid1_available, forward),
    ?assertEqual( [
        [null, null, 2  , 16 ],
        [null, null, 2  , 4  ],
        [null, null, 2  , 4  ],
        [null, null, 4  , 4  ]
    ], Grid1).

grid_rows_fwd_2_test() ->
    Grid1 = tofe_grid:slide_rows(?grid2_available, forward),
    ?assertEqual( [
        [2048, 64 , 1024, 32  ],
        [2048, 2  , 16  , 32  ],
        [null , null, 128 , 2   ],
        [null , 2  , 512 , 4   ]
    ], Grid1).

grid_rows_fwd_3_test() ->
    ?assertEqual(?grid_3_game_over, tofe_grid:slide_rows(?grid_3_game_over, forward)).


grid_rows_bwd_1_test() ->
    ?assertEqual([
        [2, 16, null, null],
        [2, 4 , null, null],
        [4, 2 , null, null],
        [4, 4 , null, null]
    ], tofe_grid:slide_rows(?grid1_available, backward)).


grid_rows_bwd_2_test() ->
    ?assertEqual([
        [2048, 64 , 1024, 32  ],
        [2048, 2  , 16  , 32  ],
        [128 , 2  , null , null ],
        [2   , 512, 4   , null ]
    ], tofe_grid:slide_rows(?grid2_available, backward)).


grid_rows_bwd_3_test() ->
    ?assertEqual(?grid_3_game_over, tofe_grid:slide_rows(?grid_3_game_over, backward)).


grid_cols_bwd_1_test() ->
    ?assertEqual([
        [4  , null, 4  , 16 ],
        [2  , null, 2  , 4  ],
        [null, null, null, 2  ],
        [null, null, null, 4  ]
    ], tofe_grid:slide_columns(?grid1_available, backward)).


grid_cols_bwd_2_test() ->
    ?assertEqual([
        [4096, 64 , 1024, 64  ],
        [128 , 2  , 16  , 2   ],
        [2   , 512, 4   , null ],
        [null , null, null , null ]
    ], tofe_grid:slide_columns(?grid2_available, backward)).


grid_cols_bwd_3_test() ->
    ?assertEqual(?grid_3_game_over, tofe_grid:slide_rows(?grid_3_game_over, backward)).


grid_cols_fwd_1_test() ->
    ?assertEqual([
        [null, null, null, 16 ],
        [null, null, null, 4  ],
        [2  , null, 2  , 2  ],
        [4  , null, 4  , 4  ]
    ], tofe_grid:slide_columns(?grid1_available, forward)).


grid_cols_fwd_2_test() ->
    ?assertEqual([
        [null , null, null , null ],
        [4096, 64 , 1024, null ],
        [128 , 2  , 16  , 64 ],
        [2   , 512, 4   , 2   ]
    ], tofe_grid:slide_columns(?grid2_available, forward)).


grid_cols_fwd_3_test() ->
    ?assertEqual(?grid_3_game_over, tofe_grid:slide_columns(?grid_3_game_over, forward)).

grid_has_remaining_moves_1_test() ->
    ?assert(tofe_grid:has_remaining_moves(?grid1_available)).

%%grid_has_remaining_moves_2_test() ->
%%    ?assert(tofe_grid:has_remaining_moves(?grid2_available)).

grid_has_remaining_moves_3_test() ->
    ?assertNot(tofe_grid:has_remaining_moves(?grid_3_game_over)).

grid_has_possible_moves_1_test() ->
    ?assert(tofe_grid:has_possible_moves(?grid1_available)).

grid_has_possible_moves_2_test() ->
    ?assert(tofe_grid:has_possible_moves(?grid2_available)).

grid_has_possible_moves_3_test() ->
    ?assertNot(tofe_grid:has_possible_moves(?grid_3_game_over)).

grid_has_empty_spaces_1_test() ->
    ?assert(tofe_grid:has_empty_spaces(?grid1_available)).

grid_has_empty_spaces_2_test() ->
    ?assert(tofe_grid:has_empty_spaces(?grid2_available)).

grid_has_empty_spaces_3_test() ->
    ?assertNot(tofe_grid:has_empty_spaces(?grid_3_game_over)).
