-module(pollution_supervisor).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(_) ->
  supervisor:start_link({local, pollution_supervisor}, ?MODULE, ok),
  unlink(whereis(pollution_supervisor)).

init(_) ->
  {ok, {#{strategy => one_for_one, intensity => 1, period => 5}, [#{id => pollution_gen_server,
    start => {pollution_gen_server, start_link, [ok]},
    restart => transient,
    shutdown => 2000,
    type => worker,
    modules => [pollution]}]}}.