-module(pollution_gen_client).
-author("Marcin Szubert").

%% API
-export([crash/0, start/0, stop/0]).
-export([addStation/2, addValue/4, removeValue/3]).
-export([getOneValue/3, getStationMean/2, getDailyMean/2, getDailyAverageDataCount/0, getTypeMinValue/1, getTypeMaxValue/1]).


addStation(Name, Coordinates) ->
  gen_server:call(pollution_gen_server, {push, addStation, [Name, Coordinates]}).

addValue(StationId, Date, Type, Value) ->
  gen_server:call(pollution_gen_server, {push, addValue, [StationId, Date, Type, Value]}).

removeValue(StationId, Date, Type) ->
  gen_server:call(pollution_gen_server, {push, removeValue, [StationId, Date, Type]}).

getOneValue(StationId, Date, Type) ->
  gen_server:call(pollution_gen_server, {pull, getOneValue, [StationId, Date, Type]}).

getStationMean(StationId, Type) ->
  gen_server:call(pollution_gen_server, {pull, getStationMean, [StationId, Type]}).

getDailyMean(Day, Type) ->
  gen_server:call(pollution_gen_server, {pull, getDailyMean, [Day, Type]}).

getDailyAverageDataCount() ->
  gen_server:call(pollution_gen_server, {pull, getDailyAverageDataCount, []}).

getTypeMinValue(Type) ->
  gen_server:call(pollution_gen_server, {pull, getTypeMinValue, [Type]}).

getTypeMaxValue(Type) ->
  gen_server:call(pollution_gen_server, {pull, getTypeMaxValue, [Type]}).

crash() ->
  gen_server:cast(pollution_gen_server, crash).

start() ->
  pollution_gen_server:start_link([]).

stop() ->
  gen_server:cast(pollution_gen_server, stop).
