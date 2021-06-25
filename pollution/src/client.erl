-module(client).
-author("Marcin Szubert").

%% API
-export([addStation/2, addValue/4, removeValue/3]).
-export([getOneValue/3, getStationMean/2, getDailyMean/2, getDailyAverageDataCount/0, getTypeMinValue/1, getTypeMaxValue/1]).


push(Function, Args) ->
  server ! {push, self(), Function, Args},
  receive
    Reply -> Reply
  after 1000 ->
    timeout
  end.

pull(Function, Args) ->
  server ! {pull, self(), Function, Args},
  receive
    Reply -> Reply
  after 1000 ->
    timeout
  end.


addStation(Name, Coordinates) ->
  push(addStation, [Name, Coordinates]).

addValue(StationId, Date, Type, Value) ->
  push(addValue, [StationId, Date, Type, Value]).

removeValue(StationId, Date, Type) ->
  push(removeValue, [StationId, Date, Type]).

getOneValue(StationId, Date, Type) ->
  pull(getOneValue, [StationId, Date, Type]).

getStationMean(StationId, Type) ->
  pull(getStationMean, [StationId, Type]).

getDailyMean(Day, Type) ->
  pull(getDailyMean, [Day, Type]).

getDailyAverageDataCount() ->
  pull(getDailyAverageDataCount, []).

getTypeMinValue(Type) ->
  pull(getTypeMinValue, [Type]).

getTypeMaxValue(Type) ->
  pull(getTypeMaxValue, [Type]).
