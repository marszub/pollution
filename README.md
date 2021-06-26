# Pollution
Erlang and Elixir practice project

### Modules
- `pollution`: Basic operations on used data structure. There are implemented adding, removing and processing measurements' data functions. 
- `client` and `server`: They are a client-server solution to handle `pollution` features on a global monitor. 
- `pollution_gen_client` and `pollution_gen_server`: As above, but using built in Erlang generic server implementation. 
- `pollution_supervisor`: A `pollution_gen_server` supervisor. 
- `pollution_gen_statem`: Adding multiple measurements in one series to the `pollution_gen_server`. Implementation of built in Erlang generic state machine. 
- `PollutionData`: An Elixir module parsing data from a `csv` file and initializing `pollution_gen_server` with them. 
