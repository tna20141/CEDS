-module(sensors).
-include("general.hrl").

-export([start_link/1]).


start_link([Node, UseCEDS, Num]) ->
	NodeStr = atom_to_list(Node),
	[Name, IP] = string:tokens(NodeStr, "@"),

	lists:foreach(fun(Seq) ->
		FullName = string:concat(Name, integer_to_list(Seq)),
		Tmp = string:concat(FullName, "@"),
		FullNodeName = string:concat(Tmp, IP),
		AtomNodeName = list_to_atom(FullNodeName),

		temperature_sensor:start_link([AtomNodeName, UseCEDS]),
		humidity_sensor:start_link([AtomNodeName, UseCEDS]),
		movement_sensor:start_link([AtomNodeName, UseCEDS]),

		ok
	end, lists:seq(1, Num)),

	ok.