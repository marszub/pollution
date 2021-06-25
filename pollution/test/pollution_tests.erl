-module(pollution_tests).
-author("Marcin Szubert").
-include_lib("eunit/include/eunit.hrl").


createMonitor_test() ->
  Expected = {dict:new(), dict:new()},
  Expected = pollution:createMonitor().


addStation_test() ->
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  ?assertNotMatch({_, Monitor1}, pollution:addStation("Stacja2", {30, 20}, Monitor1)).


addStation_sameNames_test() ->
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  ?assertMatch({error, Monitor1}, pollution:addStation("Stacja1", {30, 20}, Monitor1)).


addStation_sameCoordinates_test() ->
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  ?assertMatch({error, Monitor1}, pollution:addStation("Stacja2", {20, 20}, Monitor1)).


addAndGetValue1_test() ->
  Date = {{2000, 1, 1}, {}},
  Type = "Typ1",
  Value = 20,
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  {ok, Monitor2} = pollution:addStation("Stacja2", {30, 20}, Monitor1),
  {ok, Monitor3} = pollution:addValue("Stacja2", Date, Type, Value, Monitor2),
  ?assertEqual(Value, pollution:getOneValue("Stacja2", Date, Type, Monitor3)).


addAndGetValue2_test() ->
  Date = {{2000, 1, 1}, {}},
  Type = "Typ1",
  Value = 20,
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  {ok, Monitor2} = pollution:addStation("Stacja2", {30, 20}, Monitor1),
  {ok, Monitor3} = pollution:addValue("Stacja2", Date, Type, Value, Monitor2),
  ?assertEqual(Value, pollution:getOneValue({30, 20}, Date, Type, Monitor3)).


addAndGetValue3_test() ->
  Date = {{2000, 1, 1}, {}},
  Type = "Typ1",
  Value = 20,
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  {ok, Monitor2} = pollution:addStation("Stacja2", {30, 20}, Monitor1),
  {ok, Monitor3} = pollution:addValue({30, 20}, Date, Type, Value, Monitor2),
  ?assertEqual(Value, pollution:getOneValue({30, 20}, Date, Type, Monitor3)).


addAndGetValue4_test() ->
  Date = {{2000, 1, 1}, {}},
  Type = "Typ1",
  Value = 20,
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  {ok, Monitor2} = pollution:addStation("Stacja2", {30, 20}, Monitor1),
  {ok, Monitor3} = pollution:addValue({30, 20}, Date, Type, Value, Monitor2),
  ?assertEqual(Value, pollution:getOneValue("Stacja2", Date, Type, Monitor3)).


removeValue_test() ->
  Date = {{2000, 1, 1}, {}},
  Type = "Typ1",
  Value = 20,
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  {ok, Monitor2} = pollution:addStation("Stacja2", {30, 20}, Monitor1),
  {ok, Monitor3} = pollution:addValue({30, 20}, Date, Type, Value, Monitor2),
  {ok, Monitor4} = pollution:removeValue({30, 20}, Date, Type, Monitor3),
  ?assertMatch(error, pollution:getOneValue("Stacja2", Date, Type, Monitor4)).


getStationMean_test() ->
  Type = "Typ1",
  Value = 20,
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  {ok, Monitor2} = pollution:addStation("Stacja2", {30, 20}, Monitor1),
  {ok, Monitor3} = pollution:addValue({30, 20}, {{2000, 1, 1}, {}}, Type, Value, Monitor2),
  {ok, Monitor4} = pollution:addValue("Stacja2", {{2000, 1, 2}, {}}, Type, Value*2, Monitor3),
  {ok, Monitor5} = pollution:addValue("Stacja2", {{2000, 1, 3}, {}}, Type, Value*6, Monitor4),
  {ok, Monitor6} = pollution:addValue("Stacja1", {{2000, 1, 1}, {}}, Type, -Value*9, Monitor5),

  ?assertEqual(60.0, pollution:getStationMean("Stacja2", Type, Monitor6)).


getDailyMean_test() ->
  Type = "Typ1",
  Value = 20,
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  {ok, Monitor2} = pollution:addStation("Stacja2", {30, 20}, Monitor1),
  {ok, Monitor3} = pollution:addValue({30, 20}, {{2000, 1, 1}, {1}}, Type, Value, Monitor2),
  {ok, Monitor4} = pollution:addValue("Stacja2", {{2000, 1, 1}, {2}}, Type, Value*2, Monitor3),
  {ok, Monitor5} = pollution:addValue("Stacja2", {{2000, 1, 1}, {3}}, Type, Value*6, Monitor4),
  {ok, Monitor6} = pollution:addValue("Stacja1", {{2000, 1, 1}, {4}}, Type, -Value*12, Monitor5),

  ?assertEqual(-15.0, pollution:getDailyMean({2000, 1, 1}, Type, Monitor6)).


getDailyAverageDataCount_test() ->
  Type = "Typ1",
  Value = 20,
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  {ok, Monitor2} = pollution:addStation("Stacja2", {30, 20}, Monitor1),
  {ok, Monitor3} = pollution:addValue({30, 20}, {{2000, 1, 1}, {1}}, Type, Value, Monitor2),
  {ok, Monitor4} = pollution:addValue("Stacja2", {{2000, 1, 1}, {2}}, Type, Value*2, Monitor3),
  {ok, Monitor5} = pollution:addValue("Stacja2", {{2000, 1, 2}, {3}}, Type, Value*6, Monitor4),
  {ok, Monitor6} = pollution:addValue("Stacja1", {{2000, 1, 2}, {4}}, Type, -Value*12, Monitor5),

  ?assertEqual(2.0, pollution:getDailyAverageDataCount(Monitor6)).


getTypeMinValue_test() ->
  Type = "Typ1",
  Value = 20.0,
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  {ok, Monitor2} = pollution:addStation("Stacja2", {30, 20}, Monitor1),
  {ok, Monitor3} = pollution:addValue({30, 20}, {{2000, 1, 1}, {1}}, Type, Value, Monitor2),
  {ok, Monitor4} = pollution:addValue("Stacja2", {{2000, 1, 1}, {2}}, Type, Value*2, Monitor3),
  {ok, Monitor5} = pollution:addValue("Stacja2", {{2000, 1, 2}, {3}}, Type, Value*6, Monitor4),
  {ok, Monitor6} = pollution:addValue("Stacja1", {{2000, 1, 2}, {4}}, Type, -Value*12, Monitor5),

  ?assertEqual(-Value*12, pollution:getTypeMinValue(Type, Monitor6)).


getTypeMaxValue_test() ->
  Type = "Typ1",
  Value = 20.0,
  Monitor = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation("Stacja1", {20, 20}, Monitor),
  {ok, Monitor2} = pollution:addStation("Stacja2", {30, 20}, Monitor1),
  {ok, Monitor3} = pollution:addValue({30, 20}, {{2000, 1, 1}, {1}}, Type, Value, Monitor2),
  {ok, Monitor4} = pollution:addValue("Stacja2", {{2000, 1, 1}, {2}}, Type, Value*2, Monitor3),
  {ok, Monitor5} = pollution:addValue("Stacja2", {{2000, 1, 2}, {3}}, Type, Value*6, Monitor4),
  {ok, Monitor6} = pollution:addValue("Stacja1", {{2000, 1, 2}, {4}}, Type, -Value*12, Monitor5),

  ?assertEqual(Value*6, pollution:getTypeMaxValue(Type, Monitor6)).