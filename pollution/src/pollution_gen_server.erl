-module(pollution_gen_server).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/3]).

start_link(_) ->
  gen_server:start_link(
    {local,pollution_gen_server},
    pollution_gen_server,
    ok, []).

init(_) ->
  {ok, pollution:createMonitor()}.


handle_call({pull, Function, Args}, _, Monitor) ->
  {reply, apply(pollution, Function, Args ++ [Monitor]), Monitor};
handle_call({push, Function, Args}, _, Monitor) ->
  {Ok, NewMonitor} = apply(pollution, Function, Args ++ [Monitor]),
  {reply, Ok, NewMonitor}.

handle_cast(crash, _, Monitor) ->
  1/0;

handle_cast(stop, _, Monitor) ->
  {stop, normal, ok, Monitor}.