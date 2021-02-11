-module(rps).

%%-include_lib("hmm").

-export([play/2]).

play(P1_Input, P2_Input) when length(P1_Input) =:= length(P2_Input) ->
  deduce(P1_Input, P2_Input, 0, 0, 0);

play(P1_Input, P2_Input) when length(P1_Input) =/= length(P2_Input) ->
  size_mismatch.

deduce(_, _, P1_Wins, P2_Wins, Ties) when P1_Wins == 3; P2_Wins == 3 ->
  {P1_Wins, P2_Wins, Ties};

deduce([H1 | T1], [H2 | T2], P1_Wins, P2_Wins, Ties) ->
  case {H1, H2} of
    {Value, Value} -> deduce(T1, T2, P1_Wins, P2_Wins, Ties + 1);
    {Value1, Value2} when
      (Value1 == paper andalso Value2 == rock);
      (Value1 == rock andalso Value2 == scissors);
      (Value1 == scissors andalso Value2 == paper) -> deduce(T1, T2, P1_Wins + 1, P2_Wins, Ties);
    {_, _} -> deduce(T1, T2, P1_Wins, P2_Wins + 1, Ties)
  end;

deduce([], [], P1_Wins, P2_Wins, Ties) ->
  {P1_Wins, P2_Wins, Ties}.