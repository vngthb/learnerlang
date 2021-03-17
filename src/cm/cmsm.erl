-module(cmsm).

-export([start/0, call/1, stop/0]).
-export([init/0, prepare/3]).

-record(odata, {name, water, milk, coffee, sugar}).

%% public calls
start() ->
  register(cmp, spawn(cmsm, init, [])).

call(Command) ->
  whereis(cmp) ! Command.

stop() ->
  whereis(cmp) ! {system, stop}.
%%  exit(whereis(cmp), ok).

init() ->
  idle(initial_resources()).

%% states
idle(Resources) ->
  io:fwrite("State [IDLE].~n"),
  io:fwrite("Current resource status: ~p~n", [Resources]),
  receive
    {drink, Drink} ->
      Recipe = get_recipe(Drink),
      preparing(Resources, Recipe);
    {system, stop} ->
      io:fwrite("Stopping the coffee machine process.~n")
  end.

preparing(Resources, Recipe) ->
  io:fwrite("State [PREPARING].~n"),
  if
    Recipe#odata.sugar > 0 ->
      io:fwrite("Waiting for sugar input.~n"),
     receive
        {sugar, true} -> spawn(cmsm, prepare, [self(), Resources, Recipe]);
        {sugar, false} -> spawn(cmsm, prepare, [self(), Resources, Recipe#odata{sugar = 0}])
      after 30000 ->
        io:fwrite("Reverting to state [IDLE].~n"),
        idle(Resources)
      end;
    Recipe#odata.sugar == 0 ->
      spawn(cmsm, prepare, [self(), Resources, Recipe])
  end,
  receive
    {ready, CurrentResources} ->
      io:fwrite("[~p] is ready!~n", [Recipe#odata.name]),
      idle(CurrentResources);
    {failure, _CurrentResources} ->
      io:fwrite("[~p] preparation failed!~n", [Recipe#odata.name]),
      maintenance()
  end.

maintenance() ->
  io:fwrite("State [MAINTENANCE].~n"),
  receive
    {system, reset} ->
      io:fwrite("Resetting the coffee machine.~n"),
      idle(initial_resources());
    {system, stop} ->
      io:fwrite("Stopping the coffee machine process.~n")
  end.

%% private calls
prepare(Source, Resources, Recipe) ->
  ResourcesAvailable = are_resources_available(Resources, Recipe),
  if
    ResourcesAvailable == true ->
      NewResources = #odata{
        name = stock,
        water = Resources#odata.water - Recipe#odata.water,
        milk = Resources#odata.milk - Recipe#odata.milk,
        coffee = Resources#odata.coffee - Recipe#odata.coffee,
        sugar = Resources#odata.sugar - Recipe#odata.sugar
      },
      Source ! {ready, NewResources};
    ResourcesAvailable == false ->
      Source ! {failure, Resources}
  end.

are_resources_available(Resources, Recipe) ->
  if
    Resources#odata.water >= Recipe#odata.water,
    Resources#odata.milk >= Recipe#odata.milk,
    Resources#odata.coffee >= Recipe#odata.coffee,
    Resources#odata.sugar >= Recipe#odata.sugar -> true;
    true -> false
  end.

get_recipe(Drink) ->
  case Drink of
    water -> #odata{name = water, water = 100, milk = 0, coffee = 0, sugar = 0};
    espresso -> #odata{name = espresso, water = 100, milk = 0, coffee = 50, sugar = 5};
    cappuccino -> #odata{name = cappuccino, water = 0, milk = 50, coffee = 200, sugar = 5};
    latte -> #odata{name = latte, water = 0, milk = 300, coffee = 50, sugar = 5}
  end.

initial_resources() ->
  #odata{name = stock, water = 2000, milk = 500, coffee = 1000, sugar = 500}.

%%-define(MIN_WATER, 200).
%%-define(MIN_MILK, 0).
%%-define(MIN_COFFEE, 100).
%%-define(MIN_SUGAR, 0).

%%are_resources_available(Resources) ->
%%  if
%%    Resources#odata.water < ?MIN_WATER;
%%    Resources#odata.milk < ?MIN_MILK;
%%    Resources#odata.coffee < ?MIN_COFFEE;
%%    Resources#odata.sugar < ?MIN_SUGAR -> false;
%%    true -> true
%%  end.
