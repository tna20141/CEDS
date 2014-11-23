-module(room_condition_master_noeds).
-include("general.hrl").
-behavior(gen_server).

-export([start_link/1, init/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(CONDITION_UI_HOST, {127, 0, 0, 1}).
-define(CONDITION_UI_PORT, 1234).

-define(PAYLOAD_SIZE, 1024).
-define(TEMPERATURE_LOW, 16).
-define(TEMPERATURE_HIGH, 25).
-define(HUMIDITY_LOW, 25).
-define(HUMIDITY_HIGH, 40).


start_link(Num) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [{num_rooms, Num}], []).


init(InitData) ->
	% get the number of rooms
	NumRooms = proplists:get_value(num_rooms, InitData),

	RoomTable = ets:new(room_table, [set, public]),

	lists:foreach(fun(Seq) ->
		ets:insert(Seq, [
			{temperature, good},
			{humidity, good}
		])
	end, lists:seq(1, NumRooms)),

	{ok, [{num_rooms, Num}, {room_table, RoomTable}]}.


handle_call({event, Event}, _From, StateData) ->
	Seq = get_seq(Event),
	PrevCondition = get_room_state(Seq, StateData),

	% analyze the event
	NewState = analyze_event_and_update(Event, StateData),

	NewCondition = get_room_state(Seq, NewState),

	case NewCondition of
		% if the new state is the same as the previous state
		PrevCondition ->
			% no need for any reactions
			ok;

		% else
		_NotPrevCondition ->
			% react to this change of state
			change_room_state(Seq, PrevCondition, NewCondition)

	end,

	{reply, ok, NewState}.


get_seq(Event) ->
	<<_Value/integer, Seq/integer, _Rest/binary>> = proplists:get_value(private_data, Event),
	Seq.


% get the room condition status (good or bad)
get_status(RoomState) ->
	% get the status attributes
	TemperatureStatus = proplists:get_value(temperature, RoomState),
	HumidityStatus = proplists:get_value(humidity, RoomState),

	% the room's condition is good only if all 2 attributes are good
	Status = case TemperatureStatus of
		good ->
			case HumidityStatus of
				good ->
					good;

				bad ->
					bad

			end;

		bad ->
			bad

	end,

	Status.


% analyze the temperature value
analyze_temperature(Temperature) ->
	% check the value
	case Temperature >= ?TEMPERATURE_LOW andalso Temperature =< ?TEMPERATURE_HIGH of
		% within the predefined range
		true ->
			% temperature condition is good
			good;

		% not within the predefined range
		false ->
			% temperature condition is bad
			bad

	end.


% analyze the humidity value
analyze_humidity(Humidity) ->
	% check the value
	case Humidity >= ?HUMIDITY_LOW andalso Humidity =< ?HUMIDITY_HIGH of
		% within the predefined range
		true ->
			% humidity condition is good
			good;

		% not within the predefined range
		false ->
			% humidity condition is bad
			bad

	end.


% extract the room sequence number from the event
analyze_event_and_update(Event, StateData) ->
	<<Value/integer, Seq/integer, _Rest/binary>> = proplists:get_value(private_data, Event),
	Type = proplists:get_value(type, Event),

	Condition = case Type of
		% temperature event
		temperature ->
			% analyze the temperature value
			analyze_temperature(Value);

		% humidity event
		humidity ->
			% analyze the humidity value
			analyze_humidity(Value)
	end,

	RoomTable = proplists:get_value(room_table, StateData),
	{Seq, RoomCondition} = ets:lookup(RoomTable, Seq),

	NewRoomCondition = [{Type, Condition} | proplists:delete(Type, RoomCondition)],

	ets:insert(RoomTable, {Seq, NewRoomCondition}),

	StateData.


% get the state of the room from the data
get_room_state(Seq, StateData) ->
	RoomTable = proplists:get_value(room_table, StateData),

	{Seq, RoomCondition = ets:lookup(RoomTable, Seq),
	get_status(RoomCondition).


% payload for changing the room's state
change_room_state(Seq, PrevState, NewState) ->
	io:format("room_condition_master: #~w: ~w -> ~w~n", [Seq, PrevState, NewState]),
	% initiate connection to the UI control board
	{ok, Socket} = gen_tcp:connect(?CONDITION_UI_HOST, ?CONDITION_UI_PORT, [binary]),

	% convert the new state into a number for transmission
	StateInNumber = case NewState of
		condition_good ->
			% choose 0 as good
			0;

		condition_bad ->
			% choose 1 as bad
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
