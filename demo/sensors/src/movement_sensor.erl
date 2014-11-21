-module(movement_sensor).
-include("general.hrl").

-export([start_link/1, loop/1]).

-define(INTERVAL, 10).
-define(PAYLOAD_SIZE, 1024).
-define(POSITIVE_DURATION, (4000+random:uniform(40000))).
-define(NEGATIVE_DURATION, (random:uniform(60000))).


start_link([Node, UseCEDS]) ->
	spawn_link(?MODULE, loop, [[
		{node, Node},
		{current_state, negative},
		{use_ceds, UseCEDS},
		{events_left, ?NEGATIVE_DURATION div ?INTERVAL}
	]]).


loop(LoopData) ->
	Node = proplists:get_value(node, LoopData),
	{Movement, NewLoopData} = gen_movement_value(LoopData),

	Event = create_movement_event(Movement),

	gen_movement_event(Event, Node, proplists:get_value(use_ceds, LoopData)),

	timer:sleep(?INTERVAL),
	loop(NewLoopData).


create_movement_event(Movement) ->
	Data = utils:create_data(?PAYLOAD_SIZE),

	case Movement of
		positive ->
			utils:create_event(movement_positive, Data, false);

		negative ->
			utils:create_event(movement_negative, Data, false)

	end.


gen_movement_event(Event, Node, UseCEDS) ->
	case UseCEDS of
		true ->
			gen_server:call({?LOCAL_PROXY, Node}, {event, Event});

		false ->
			gen_fsm:call({room_condition_whatever, Node}, {event, Event})

	end.


gen_movement_value(LoopData) ->
	EventsLeft = proplists:get_value(events_left, LoopData),

	case EventsLeft > 0 of
		true ->
			NewLoopData = [{events_left, EventsLeft-1} | proplists:delete(events_left, LoopData)],
			{proplists:get_value(current_state, LoopData), NewLoopData};

		false ->
			PrevState = proplists:get_value(current_state, LoopData),
			{NewEventsLeft, NewState} = case PrevState of
				negative ->
					{?POSITIVE_DURATION div ?INTERVAL, positive};

				positive ->
					{?NEGATIVE_DURATION div ?INTERVAL, negative}
			end,

			NewLoopData1 = [{events_left, NewEventsLeft} | proplists:delete(events_left, LoopData)],
			NewLoopData2 = [{current_state, NewState} | proplists:delete(current_state, NewLoopData1)],
			{NewState, NewLoopData2}

	end.