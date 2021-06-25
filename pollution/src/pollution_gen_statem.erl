-module(pollution_gen_statem).

-export([start_link/0, init/1, callback_mode/0, stop/0]).
-export([stationChoice/3, addingValues/3]).
-export([set_station/1, add_value/3, store_data/0]).

addMany(_, []) ->
  ok;
addMany(StationId, [{Date, Type, Value}|Tab]) ->
  pollution_gen_client:addValue(StationId, Date, Type, Value),
  addMany(StationId, Tab).



%% PUBLIC API
set_station(StationID) -> gen_statem:call(value_statem, {set, StationID}).
add_value(Date, Type, Value) -> gen_statem:call(value_statem, {add, {Date, Type, Value}}).
store_data() -> gen_statem:call(value_statem, store).

start_link() ->
  gen_statem:start_link({local, value_statem}, ?MODULE, [], []).
init([]) -> {ok, stationChoice, []}.
callback_mode()->state_functions.

%% HANDLERS
stationChoice({call, From}, {set, StationId}, _) -> {next_state, addingValues, {StationId, []}, [{reply,From,addingValues}]}.
addingValues({call, From}, {add, Data}, {StationId, Tab}) -> {next_state, addingValues, {StationId, [Data|Tab]}, [{reply,From,addingValues}]};
addingValues({call, From}, store, {StationId, Tab}) ->
  addMany(StationId, Tab),
  {next_state, stationChoice, [], [{reply,From,stationChoice}]}.

stop() -> gen_statem:stop(value_statem).