-module(er_app).
-behavior(application).
-include("general.hrl").

-export([start/2, stop/1]).


% start the application
start(_Type, _StartArgs) ->
	% start the supervisor tree
	er_sup:start_link().


% stop the application
stop(_State) ->
	ok.
