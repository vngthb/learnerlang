-module(cmsup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupervisorSpecs = #{
    strategy => one_for_one,
    intensity => 5,
    period => 30
  },

  CoffeeMachine = #{
    id => cm1,
    start => {cmgs, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [dynamic]
  },

  {ok,
    {SupervisorSpecs,
      [CoffeeMachine]}}.
