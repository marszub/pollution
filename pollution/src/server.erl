-module(server).
-author("Marcin Szubert").
%% API
-export([start/0, stop/0, init/0]).

-import(pollution, [createMonitor/0]).

start() ->
  register(server, spawn(?MODULE, init, [])).


stop() ->
  server ! stop.


init() ->
  State = createMonitor(),
  loop(State).


loop(State) ->
  receive
    stop -> ok;
    {pull, Pid, Function, Args} when is_list(Args) ->
      Pid ! apply(pollution, Function, Args ++ [State]),
      loop(State);
    {push, Pid, Function, Args} when is_list(Args) ->
      {Ok, NewState} = apply(pollution, Function, Args ++ [State]),
      Pid ! Ok,
      loop(NewState)
  end.
