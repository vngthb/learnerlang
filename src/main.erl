-module(main).

-export([all/2, big/1, dropwhile/2, foldl/3]).

-include("misc/record_test.hrl").

big(X) ->
  if X > 50 -> true;
    X =< 50 -> false
  end.

dropwhile(P, [H | T] = Rest) ->
  case P(H) of
    true -> dropwhile(P, T);
    false -> Rest
  end.

foldl(P, A, [H | T]) ->
  foldl(P, P(A, H), T);

foldl(_, A, []) ->
  A.

%%%%%%%%%%%%%%%%%%%%%

all(P, [H | T]) ->
  case P(H) of
    true -> all(P, T);
    false -> false
  end;

all(_, []) ->
  true.

mapfoldl() ->
  ok.