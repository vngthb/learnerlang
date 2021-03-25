-module(cmgs).

-behaviour(gen_server).

-export([start_link/0, order/2, status/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(odata, {name, water, milk, coffee, sugar}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

order(Drink, Sugar) ->
  gen_server:cast(?SERVER, {Drink, Sugar}).

status() ->
  gen_server:call(?SERVER, {system, status}).

stop() ->
  gen_server:call(?SERVER, {system, stop}).

%%%===================================================================
%%% Callback functions
%%%===================================================================

init([]) ->
  {ok, {idle, initial_resources()}}.

handle_cast({water, _Sugar}, {idle, Resources}) ->
  Recipe = get_recipe(water),
  {Result, NewResources} = prepare(Resources, Recipe),
  if
    Result == ready -> {noreply, {idle, NewResources}};
    Result == failure -> {noreply, {maintenance, Resources}}
  end;
handle_cast({Drink, Sugar}, {idle, Resources}) ->
  Recipe = get_recipe(Drink),
  {Result, NewResources} = if
                             Sugar == true -> prepare(Resources, Recipe);
                             Sugar == false -> prepare(Resources, Recipe#odata{sugar = 0})
                           end,
  if
    Result == ready -> {noreply, {idle, NewResources}};
    Result == failure -> {noreply, {maintenance, Resources}}
  end;
handle_cast(_Request, Status) ->
  {noreply, Status}.

handle_call({system, reset}, _From, {maintenance, _Resources}) ->
  {reply, reset, initial_resources()};
handle_call({system, stop}, _From, {State, _Resources} = Status) when State == idle; State == maintenance ->
  {stop, normal, ok, Status};
handle_call({system, status}, _From, Status) ->
  {reply, Status, Status}.

handle_info(_Request, Status) ->
  {noreply, Status}.

terminate(_Reason, _Status) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

prepare(Resources, Recipe) ->
  ResourcesAvailable = are_resources_available(Resources, Recipe),
  if
    ResourcesAvailable == true ->
      NewResources = #odata{
        name = resources,
        water = Resources#odata.water - Recipe#odata.water,
        milk = Resources#odata.milk - Recipe#odata.milk,
        coffee = Resources#odata.coffee - Recipe#odata.coffee,
        sugar = Resources#odata.sugar - Recipe#odata.sugar
      },
      {ready, NewResources};
    ResourcesAvailable == false ->
      {failure, Resources}
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
  #odata{name = resources, water = 2000, milk = 500, coffee = 1000, sugar = 500}.