-module(sensors).
-include("general.hrl").

-export([start_link/1]).


start_link([Node, UseCEDS, Num]) ->
	NodeStr = atom_to_list(Node),
	[Name, IP] = string:tokens(NodeStr, "@"),

	lists:foreach(fun(Seq) ->
		FinalNodeName = case UseCEDS of
			true ->
				FullName = string:concat(Name, integer_to_list(Seq)),
				Tmp = string:concat(FullName, "@"),
				FullNodeName = string:concat(Tmp, IP),
				list_to_atom(FullNodeName);

			false ->
				Node
		end,

		temperature_sensor:start_link([FinalNodeName, UseCEDS, Seq]),
		humidity_sensor:start_link([FinalNodeName, UseCEDS, Seq]),
		movement_sensor:start_link([FinalNodeName, UseCEDS, Seq]),

		ok
	end, lists:seq(1, Num)),

	ok.