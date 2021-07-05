-module(tofe_vector_test).

-include_lib("eunit/include/eunit.hrl").


%% @doc slide forward
slide_forward_test() ->
    ?assertEqual([null, null, null, 2], tofe_vector:slide([null, 2, null, null], forward)),
    ?assertEqual([null, null, null, 4], tofe_vector:slide([2, 2, null, null], forward)),

    ?assertEqual([null, 2, 16, 32], tofe_vector:slide([2, 16, 16, 16], forward)),
    ?assertEqual([null, null, null, 512], tofe_vector:slide([256, 256, null, null], forward)),
    ?assertEqual([16, 8, 4, 2], tofe_vector:slide([16, 8, 4, 2], forward)).


slide_backward_test() ->
    ?assertEqual([2, null, null, null], tofe_vector:slide([null, 2, null, null], backward)),
    ?assertEqual([4, null, null, null], tofe_vector:slide([2, 2, null, null], backward)),

    ?assertEqual([2, 32, 16, null], tofe_vector:slide([2, 16, 16, 16], backward)),
    ?assertEqual([512, null, null, null], tofe_vector:slide([256, 256, null, null], backward)),
    ?assertEqual([16, 8, 4, 2], tofe_vector:slide([16, 8, 4, 2], backward)).
