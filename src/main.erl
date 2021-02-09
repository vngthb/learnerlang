-module(main).

-export([greet/1]).

greet(Name) ->
  io:fwrite("Greetings, ~s/n!").
