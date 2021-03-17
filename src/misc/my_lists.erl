-module(my_lists).

-export([
  distinct/1,
  member_of/2,
  reverse/1,
  selection_sort/1,
  filter_sort/1,
  index_of/2,
  occurrence/2,
  first_non_repeating_element/1,
  dropwhile/2,
  foldl/3,
  all/2,
  mapfoldl/3
  , size_of/1]).

%% distinct values api

distinct(List) ->
  distinct(List, []).

distinct([H | T], Temp) ->
  case member_of(H, Temp) of
    true -> distinct(T, Temp);
    false -> distinct(T, [H | Temp])
  end;

distinct([], Temp) ->
  reverse(Temp).

%% member of api

member_of(Element, [H | T]) ->
  if
    Element =:= H -> true;
    Element =/= H -> member_of(Element, T)
  end;

member_of(_, []) ->
  false.

%% reverse list api

reverse(List) ->
  reverse(List, []).

reverse([H | T], Temp) ->
  reverse(T, [H | Temp]);

reverse([], Temp) ->
  Temp.

%% sort api

selection_sort([H | T]) ->
  selection_sort(T, H, [], []);

selection_sort([]) ->
  [].

selection_sort([], Current, Temp, [H | T]) ->
  selection_sort(T, H, [Current | Temp], []);

selection_sort([H | T], Current, Temp, Leftover) ->
  if H =< Current -> selection_sort(T, Current, Temp, [H | Leftover]);
    H > Current -> selection_sort(T, H, Temp, [Current | Leftover])
  end;

selection_sort([], Current, Temp, []) ->
  [Current | Temp].

%% sort 2 api

filter_sort(L) ->
  filter_sort(L, []).

filter_sort([H | T] = L, A) ->
  S = [X || X <- L, H > X],
  B = [X || X <- L, H =< X],
  if
    S == [] -> filter_sort(T, A ++ [H]);
    S =/= [] -> filter_sort(S ++ B, A)
  end;

filter_sort([], A) ->
  A.

%% element occurrence api

occurrence(Element, List) ->
  occurrence(Element, List, 0).

occurrence(Element, [H | T], Counter) ->
  if
    Element =:= H -> occurrence(Element, T, Counter + 1);
    Element =/= H -> occurrence(Element, T, Counter)
  end;

occurrence(_, [], Counter) ->
  Counter.

%% index of api

index_of(Element, List) ->
  index_of(Element, List, 1).

index_of(Element, [H | T], Index) ->
  case Element =:= H of
    true -> Index;
    false -> index_of(Element, T, Index + 1)
  end;

index_of(_, [], _) ->
  list_is_empty.

%% first non repeating element api

first_non_repeating_element(List) ->
  first_non_repeating_element(List, List).

first_non_repeating_element([H | T], Initial) ->
  case occurrence(H, Initial) of
    1 -> {H, index_of(H, Initial)};
    _ -> first_non_repeating_element(T, Initial)
  end;

first_non_repeating_element([], []) ->
  list_is_empty.

%% dropwhile api

dropwhile(P, [H | T] = Rest) ->
  case P(H) of
    true -> dropwhile(P, T);
    false -> Rest
  end.

%% foldl api
%% example -> my_lists:foldl(fun(A, X) -> A+X end, 0, [1,2,3,4,5,6]).

foldl(P, A, [H | T]) ->
  foldl(P, P(A, H), T);

foldl(_, A, []) ->
  A.
%% all api

all(P, [H | T]) ->
  case P(H) of
    true -> all(P, T);
    false -> false
  end;

all(_, []) ->
  true.

%% mapfoldl api
%% example -> my_lists:mapfoldl(fun(X, S) -> {2*X, X+S} end, 0, [1,2,3,4,5]).

mapfoldl(_, A, [])
  -> {[], A};

mapfoldl(P, A, [H | T]) ->
  {R, A1} = P(H, A),
  {Rs, A2} = mapfoldl(P, A1, T),
  {[R | Rs], A2}.

size_of(L) ->
  size_of(L, 0).

size_of([_H | T] = L, C) ->
  if L =/= [] -> size_of(T, C + 1);
    L == [] -> C
  end;

size_of([], C) ->
  C.