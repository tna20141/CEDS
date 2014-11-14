-module(ed_app).
-behavior(application).
-include("general.hrl").

-export([start/2, stop/1]).


% start the application
start(_Type, StartArgs) ->
	% get the configuration file location
	ConfigFile = proplists:get_value(config_file, StartArgs),

	% start the supervisor tree
	ed_sup:start_link(ConfigFile).


% stop the application
stop(_State) ->
	ok.
