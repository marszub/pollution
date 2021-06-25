-module(server_tests).
-author("Marcin Szubert").
-include_lib("eunit/include/eunit.hrl").


startStop_test() ->
  server:start(),
  server:stop().


addStation_test() ->
  server:start(),

  ?assertEqual(ok, client:addStation("Stacja1", {10, 20})),

  server:stop().


addValue_test() ->
  server:start(),

  client:addStation("Stacja1", {10, 20}),
  ?assertEqual(ok, client:addValue("Stacja1", {{1, 1, 2020}, 12345}, "typ1", 20.3)),

  server:stop().


stop_test() ->
  server:start(),

  client:addStation("Stacja1", {10, 20}),
  client:addValue("Stacja1", {{1, 1, 2020}, 12345}, "typ1", 20.3),

  server:stop(),

  ?assertEqual(timeout, client:getOneValue("Stacja1", {{1, 1, 2020}, 12345}, "typ1")).


getOneValue_test() ->
  server:start(),

  client:addStation("Stacja1", {10, 20}),
  client:addValue("Stacja1", {{1, 1, 2020}, 12345}, "typ1", 20.3),
  ?assertEqual(20.3, client:getOneValue("Stacja1", {{1, 1, 2020}, 12345}, "typ1")),

  server:stop().