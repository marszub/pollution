-module(pollution).
-author("Marcin Szubert").
-include_lib("eunit/include/eunit.hrl").
-export([createMonitor/0]).
-export([addStation/3, addValue/5, removeValue/4]).
-export([getOneValue/4, getStationMean/3, getDailyMean/3, getDailyAverageDataCount/1, getTypeMinValue/2, getTypeMaxValue/2]).


%% returns: {CoordinatesToName, NameToStation}
createMonitor() ->
  {dict:new(), dict:new()}.


%% returns: {Name, Coordinates, List measurements}
createStation(Name, Coordinates) ->
  {Name, Coordinates, dict:new()}.


%% returns: Name or error
getStationName(Coordinates, {CoordinatesToName, _}) when is_tuple(Coordinates)->
  Find = dict:find(Coordinates, CoordinatesToName),
  case Find of
    {ok, Name} -> Name;
    error -> error
  end.


%% returns: Station or error
getStation(Name, {_, NameToStation}) when is_list(Name) ->
  Find = dict:find(Name, NameToStation),
  case Find of
    {ok, Station} -> Station;
    error -> error
  end;
getStation(Coordinates, Monitor) when is_tuple(Coordinates) ->
  getStation(getStationName(Coordinates, Monitor), Monitor);
getStation(_, _) ->
  error.


%% returns: {ok, Updated monitor} or {error, Not updated monitor}
updateStation(Station, {CoordinatesToName, NameToStation})->
  {Name, _, _} = Station,
  Exists = dict:is_key(Name, NameToStation),
  if
    Exists -> {ok, {CoordinatesToName, dict:store(Name, Station, NameToStation)}};
    true -> {error, {CoordinatesToName, NameToStation}}
  end.


%% returns: {ok, Updated monitor} or {error, Not updated monitor}
addStation(Name, Coordinates, Monitor) ->
  {CoordinatesToName, NameToStation} = Monitor,
  Exists = dict:is_key(Coordinates, CoordinatesToName) or dict:is_key(Name, NameToStation),
  if
  Exists -> {error, Monitor};
  true -> {ok, {dict:store(Coordinates, Name, CoordinatesToName), dict:store(Name, createStation(Name, Coordinates), NameToStation)}}
  end.


%% returns: {ok, Updated dict} or {error, Not updated dict}
addMeasurement(Date, Type, Value, Measurements) ->
  Exists = dict:is_key({Date, Type}, Measurements),
  if
    Exists -> {error, Measurements};
    true -> {ok, dict:store({Date, Type}, Value, Measurements)}
  end.


%% returns: Value or error
getMeasurement(Date, Type, Measurements) ->
  Find = dict:find({Date, Type}, Measurements),
  case Find of
    {ok, Value} -> Value;
    error -> error
  end.


%% returns: {ok, Updated dict} or {error, Not updated dict}
removeMeasurement(Date, Type, Measurements) ->
  Exists = dict:is_key({Date, Type}, Measurements),
  if
    Exists -> {ok, dict:erase({Date, Type}, Measurements)};
    true -> {error, Measurements}
  end.


%% returns: {ok, Updated monitor} or {error, Not updated monitor}
addValue(StationId, Date, Type, Value, Monitor) ->
  Station = getStation(StationId, Monitor),
  case Station of
    error -> {error, Monitor};
    {Name, Coordinates, Measurements} ->
      {Ok, NewMeasurements} = addMeasurement(Date, Type, Value, Measurements),
      case Ok of
        ok ->
          updateStation({Name, Coordinates, NewMeasurements}, Monitor);
        error ->
          {error, Monitor}
      end
  end.


%% returns: {ok, Updated monitor} or {error, Not updated monitor}
removeValue(StationId, Date, Type, Monitor) ->
  Station = getStation(StationId, Monitor),
  case Station of
    error -> {error, Monitor};
    {Name, Coordinates, Measurements} ->
      {Ok, NewMeasurements} = removeMeasurement(Date, Type, Measurements),
      case Ok of
        ok -> updateStation({Name, Coordinates, NewMeasurements}, Monitor);
        error -> {error, Monitor}
      end
  end.



%% returns: Value or error
getOneValue(StationId, Date, Type, Monitor) ->
  Station = getStation(StationId, Monitor),
  case Station of
    error -> error;
    {_, _, Measurements} -> getMeasurement(Date, Type, Measurements)
  end.


%% returns: Value or error
getStationMean(StationId, Type, Monitor) ->
  Station = getStation(StationId, Monitor),
  case Station of
    error ->
      error;
    {_, _, Measurements} ->
      Values = lists:map(fun({_, Value}) -> Value end, lists:filter(fun({{_, T}, _}) ->  T == Type end, dict:to_list(Measurements))),
      lists:sum(Values) / length(Values)
  end.


%% returns: [Value]
getDayMeasurementsFromStation(Day, Type, {_, _, Measurements}) ->
  lists:map(fun({_, Value}) -> Value end, lists:filter(fun({{{D, _}, T}, _}) ->  (D == Day) and (T == Type) end, dict:to_list(Measurements))).


%% returns: Value
getDailyMean(Day, Type, {_, NameToStation}) ->
  ListOfStations = lists:map(fun({_, Value}) -> Value end, dict:to_list(NameToStation)),
  Values = lists:flatten(lists:map(fun(Station) -> getDayMeasurementsFromStation(Day, Type, Station) end, ListOfStations)),
  lists:sum(Values) / length(Values).


%% returns: Value
getDailyAverageDataCount({_, NameToStation}) ->
  ListOfStations = dict:to_list(NameToStation),
  StationsDataCount = lists:map(fun({_, {_, _, Measurements}}) -> length(dict:to_list(Measurements)) end, ListOfStations),
  lists:sum(StationsDataCount) / length(ListOfStations).


%% returns: Value
getStationMinOfType(Type, {_, _, Measurements}) ->
  List = dict:to_list(Measurements),
  Filtered = lists:filter(fun({{_, T}, _}) ->  T == Type end, List),
  Values = lists:map(fun({_, Value}) -> Value end, Filtered),
  lists:min(Values).


%% returns: Value
getTypeMinValue(Type, {_, NameToStation}) ->
  List = dict:to_list(NameToStation),
  StationsMin = lists:map(fun({_, Station}) -> getStationMinOfType(Type, Station) end, List),
  lists:min(StationsMin).


%% returns: Value
getStationMaxOfType(Type, {_, _, Measurements}) ->
  List = dict:to_list(Measurements),
  Filtered = lists:filter(fun({{_, T}, _}) ->  T == Type end, List),
  lists:max(lists:map(fun({_, Value}) -> Value end, Filtered)).


%% returns: Value
getTypeMaxValue(Type, {_, NameToStation}) ->
  List = dict:to_list(NameToStation),
  StationsMax = lists:map(fun({_, Station}) -> getStationMaxOfType(Type, Station) end, List),
  lists:max(StationsMax).

