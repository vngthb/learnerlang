-module(my_list).

-export([distinct/1, member_of/2, reverse/1]).

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