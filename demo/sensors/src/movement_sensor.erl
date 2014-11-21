-module(movement_sensor).
-include("general.hrl").

-export([start_link/1, loop/1]).

-define(INTERVAL, 10).
-define(PAYLOAD_SIZE, 1024).


start_link([Node, UseCEDs]) ->
	spawn_link(?MODULE, loop, [[
		{node, Node},
		{current_state, negative},
		{use_ceds, UseCEDs}
	]]).


loop(LoopData) ->
	Node = proplists:get_value(node, LoopData),
	{Movement, NewLoopData} = gen_movement_value(LoopData),

	Event = create_movement_event(Movement),

	gen_movement_event(Event, Node, proplists:get_value(use_ceds, LoopData)),

	timer:sleep(?INTERVAL),
	loop(NewLoopData).


create_movement_event(Movement) ->
	Data = utils:create(?PAYLOAD_SIZE),

	case Movement of
		positive ->
			utils:create_event(movement_postive, Data, false);

		negative ->
			utils:create_event(movement_negative, Data, false)

	end.


gen_movement_event(Event, Node, UseCEDs) ->
	case UseCEDS of
		true ->
			gen_server:call({?LOCAL_PROXY, Node}, {event, Event});

		false ->
			gen_fsm:call({room_condition_whatever, Node}, {event, Event})

	end.


gen_movement_value(LoopData) ->
	% to be implemented
	ok.