-module(rps).

-export([rps/2]).

rps(P1_Input, P2_Input) when length(P1_Input) =:= length(P2_Input) ->
  deduce(P1_Input, P2_Input, 0, 0, 0);

rps(P1_Input, P2_Input) when length(P1_Input) =/= length(P2_Input) ->
  input_lists_must_have_same_size.

deduce([paper | T1], [rock | T2], P1_Wins, P2_Wins, Draws) ->
  deduce(T1, T2, P1_Wins + 1, P2_Wins, Draws);

deduce([rock | T1], [scissors | T2], P1_Wins, P2_Wins, Draws) ->
  deduce(T1, T2, P1_Wins + 1, P2_Wins, Draws);

deduce([scissors | T1], [paper | T2], P1_Wins, P2_Wins, Draws) ->
  deduce(T1, T2, P1_Wins + 1, P2_Wins, Draws);

deduce([rock | T1], [paper | T2], P1_Wins, P2_Wins, Draws) ->
  deduce(T1, T2, P1_Wins, P2_Wins + 1, Draws);

deduce([paper | T1], [scissors | T2], P1_Wins, P2_Wins, Draws) ->
  deduce(T1, T2, P1_Wins, P2_Wins + 1, Draws);

deduce([scissors | T1], [rock | T2], P1_Wins, P2_Wins, Draws) ->
  deduce(T1, T2, P1_Wins, P2_Wins + 1, Draws);

deduce([Value | T1], [Value | T2], P1_Wins, P2_Wins, Draws) ->
  deduce(T1, T2, P1_Wins, P2_Wins, Draws + 1);

deduce(_, _, P1_Wins, _, _) when P1_Wins == 3 ->
  io:format("Player 1 wins by scoring ~B points!~n", [P1_Wins]);

deduce(_, _, _, P2_Wins, _) when P2_Wins == 3 ->
  io:format("Player 2 wins by scoring ~B points!~n", [P2_Wins]);

deduce([], [], P1_Wins, P2_Wins, Draws) ->
  {player_1_wins, P1_Wins, player_2_wins, P2_Wins, Draws}.