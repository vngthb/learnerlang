-module(rps_tests).

-include_lib("eunit/include/eunit.hrl").

-import(rps, [play/2]).

-export([]).

fail_game_when_player_1_input_list_is_larger_test() ->
  Expected = size_mismatch,
  Actual = rps:play([rock, paper, scissors, rock], [scissors, rock, paper]),
  ?assert(Expected == Actual).

fail_game_when_player_2_input_list_is_larger_test() ->
  Expected = size_mismatch,
  Actual = rps:play([rock, paper, scissors], [scissors, rock, paper, rock]),
  ?assert(Expected == Actual).

continue_on_empty_input_lists_test() ->
  Expected = {0, 0, 0},
  Actual = rps:play([],[]),
  ?assertEqual(Expected, Actual).

player_1_wins_by_scoring_3_points_test() ->
  Expected = 3,
  {Actual, _, _} = rps:play([rock, paper, scissors, paper, rock], [scissors, rock, paper, rock, scissors]),
  ?assertEqual(Expected, Actual).

player_2_wins_by_scoring_3_points_test() ->
  Expected = 3,
  {_, Actual, _} = rps:play([scissors, rock, paper, rock, scissors], [rock, paper, scissors, scissors, paper]),
  ?assertEqual(Expected, Actual).

end_game_with_a_tie_test() ->
  Expected = 1,
  {_, _, Actual} = rps:play([scissors, rock, paper], [rock, rock, scissors]),
  ?assertEqual(Expected, Actual).