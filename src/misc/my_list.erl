-module(my_list).

-export([distinct/1, member_of/2, reverse/1, sort/1, index_of/2, occurrence/2, first_non_repeating_element/1]).

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

sort([H | T]) ->
  sort(T, H, [], []);

sort([]) ->
  [].

sort([], Current, Temp, [H | T]) ->
  sort(T, H, [Current | Temp], []);

sort([H | T], Current, Temp, Leftover) ->
  if H =< Current -> sort(T, Current, Temp, [H | Leftover]);
    H > Current -> sort(T, H, Temp, [Current | Leftover])
  end;

sort([], Current, Temp, []) ->
  [Current | Temp].

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

