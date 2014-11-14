-module(humidity_sensor).
-include("general.hrl").

-export([start_link/1, loop/1]).

-define(INTERVAL, 10).
-define(PAYLOAD_SIZE, 1024).
-define(GOOD_DURATION, (7000+random:uniform(2000))).
-define(BAD_DURATION, (1000+random:uniform(1000))).
-define(GOOD_VALUE, 30).
-define(BAD_VALUE, 60).


start_link(Node) ->
	spawn_link(?MODULE, loop, [[
		{node, Node},
		{current_state, good},
		{events_left, ?GOOD_DURATION div ?INTERVAL},
		{value, ?GOOD_VALUE}
	]]).


loop(LoopData) ->
	Node = proplists:get_value(node, LoopData),
	{TempValue, NewLoopData} = gen_humidity_value(LoopData),

	Event = create_humidity_event(TempValue),

	gen_humidity_event(Event, Node),

	timer:sleep(?INTERVAL),
	loop(NewLoopData).


create_humidity_event(TempValue) ->
	Data = utils:create_data(?PAYLOAD_SIZE, [TempValue]),
	utils:create_event(humidity, Data, false).


gen_humidity_event(Event, Node) ->
	gen_server:call({?LOCAL_PROXY, Node}, {event, Event}).


gen_humidity_value(LoopData) ->
	EventsLeft = proplists:get_value(events_left, LoopData),

	NewLoopData = case EventsLeft > 0 of
		true ->
			[{events_left, EventsLeft - 1} | proplists:delete(events_left, LoopData)];

		false ->
			CurrentState = proplists:get_value(current_state, LoopData),
			case CurrentState of
				good ->
					NewLoopData1 = [{current_state, bad} | proplists:delete(current_state, LoopData)],
					NewLoopData2 = [
						{events_left, ?BAD_DURATION div ?INTERVAL} |
						proplists:delete(events_left, NewLoopData1)
					],
					[{value, ?BAD_VALUE} | proplists:delete(value, NewLoopData2)];

				bad ->
					NewLoopData1 = [{current_state, good} | proplists:delete(current_state, LoopData)],
					NewLoopData2 = [
						{events_left, ?GOOD_DURATION div ?INTERVAL} |
						proplists:delete(events_left, NewLoopData1)
					],
					[{value, ?GOOD_VALUE} | proplists:delete(value, NewLoopData2)]

			end
	end,

	{proplists:get_value(value, NewLoopData), NewLoopData}.
