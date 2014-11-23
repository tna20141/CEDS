-module(room_presence_master_noeds).
-include("general.hrl").
-behavior(gen_server).

-export([start_link/1, init/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(CONDITION_UI_HOST, {127, 0, 0, 1}).
-define(CONDITION_UI_PORT, 1235).

-define(POSITIVE_TIMEOUT, 4).
-define(PAYLOAD_SIZE, 1024).


start_link(Num) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [{num_rooms, Num}], []).


init(InitData) ->
	% get the number of rooms
	NumRooms = proplists:get_value(num_rooms, InitData),

	RoomTable = ets:new(room_table, [set, public]),

	lists:foreach(fun(Seq) ->
		ets:insert(Seq, [
			{movement, negative},
			{last_update, calendar:local_time()},
			{presence, negative}
		])
	end, lists:seq(1, NumRooms)),

	{ok, [{num_rooms, Num}, {room_table, RoomTable}]}.


handle_call({event, Event}, _From, StateData) ->
	Seq = get_seq(Event),
	PrevPresence = get_presence_state(Seq, StateData),

	% analyze the event and update state data
	NewStateData = analyze_event_and_update(Event, StateData),

	NewPresence = get_presence_state(Seq, NewStateData),

	case NewPresence of
		PrevPresence ->
			ok;

		_NotPrevPresence ->
			change_room_state(Seq, PrevPresence, NewPresence)

	end,

	{reply, ok, NewStateData}.


get_seq(Event) ->
	<<Seq/integer, _Rest/binary>> = proplists:get_value(private_data, Event),
	Seq.


get_presence_state(Seq, StateData) ->
	RoomTable = proplists:get_value(room_table, StateData),

	{Seq, RoomState} = ets:lookup(RoomTable, Seq),
	proplists:get_value(presence, RoomState).


analyze_event_and_update(Event, StateData) ->
	<<Seq/integer, _Rest/binary>> = proplists:get_value(private_data, Event),
	Type = proplists:get_value(type, Event),

	RoomTable = proplists:get_value(room_table, StateData),
	{Seq, RoomState} = ets:lookup(RoomTable, Seq),

	NewRoomState = analyze_movement(Type, RoomState),

	ets:insert(RoomTable, {Seq, NewRoomState}),

	StateData.


analyze_movement(Type, RoomState) ->
	OldMovement = proplists:get_value(movement, RoomTable),

	NewRoomState = case OldMovement of
		positive ->
			case Type of
				% event implies that there are movements in the room
				movement_positive ->
					% nothing new, just continue monitoring
					RoomState;

				% event implies that there are no movements in the room
				movement_negative ->
					% update the presence state
					New1 = [{movement, negative} | proplists:delete(movement, RoomState)],

					% update time
					[{last_update, calendar:local_time()} |
							proplists:delete(last_update, New1)]
			end;

		negative ->
			OldPresence = proplists:get_value(presence, RoomState),
			case Type of
				% event implies that there are movements in the room
				movement_positive ->
					% change movement state back to positive
					New1 = [{movement, positive} | proplists:delete(movement, RoomState)],

					% update time
					New2 = [{last_update, calendar:local_time()} |
							proplists:delete(last_update, New1)],

					case OldPresence of
						% if the presence state is currently negative
						negative ->
							% change it back to positive
							[{presence, positive} | proplists:delete(presence, New2)];

						% if the presence state is currently positive
						positive ->
							New2

					end;

				% event implies that there are no movements in the room
				movement_negative ->
					case OldPresence of
						% if the presence state is already negative
						negative ->
							RoomState;

						% if the presence state is positive
						positive ->
							% check the last update time to see the how long we've been negative
							LastUpdateTime = proplists:get_value(last_update, RoomState),
							TimeDiff = get_timediff_in_seconds(LastUpdateTime, calendar:local_time()),

							% compare with the positive timeout value
							case TimeDiff >= ?POSITIVE_TIMEOUT of
								% if in negative long enough
								true ->
									% change presence state to negative
									[{presence, negative} | proplists:delete(presence, StateData)];

								% not long enough
								false ->
									% continue monitoring
									RoomState

							end

					end

			end
	end.


% get the time difference between 2 datetime structures in seconds
get_timediff_in_seconds(TimeBefore, TimeAfter) ->
	% get time difference
	{Days, Time} = calendar:time_difference(TimeBefore, TimeAfter),

	% convert it to number of seconds
	Seconds = calendar:time_to_seconds(Time),
	86400*Days+Seconds.


% payload for changing the room's state
change_room_state(Seq, PrevState, NewState) ->
	io:format("room_presence_master: #~w: ~w -> ~w~n", [Seq, PrevState, NewState]),
	% initiate connection to the UI control board
	{ok, Socket} = gen_tcp:connect(?CONDITION_UI_HOST, ?CONDITION_UI_PORT, [binary]),

	% convert the new state into a number for transmission
	StateInNumber = case NewState of
		presence_negative ->
			% choose 0 as negative
			0;

		presence_positive ->
			% choose 1 as positive
			1

	end,

	gen_tcp:send(Socket, <<Seq:8, StateInNumber:8>>),

	% close the socket
	gen_tcp:close(Socket),
	ok.


% handle stop messages
handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.


% termination cleanup callback
terminate(_Reason, _LoopData) ->
	ok.


% code_change callback function
code_change(_OldVsn, _State, _Extra) ->
	{ok, no_state}.


% handle_info callback function
handle_info(_Info, _State) ->
	{noreply, no_state}.
