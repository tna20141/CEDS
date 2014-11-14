-module(room_condition_master).
-include("general.hrl").
-behavior(gen_fsm).

-export([start_link/1, init/1]).
-export([monitoring/2]).
-export([handle_info/3, handle_sync_event/4, handle_event/3, code_change/4, terminate/3]).


% start the fsm, input is the number of rooms to monitor
start_link(Num) ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [{num_rooms, Num}], []).


% initialize the fsm
init(InitData) ->
	% get the number of rooms
	NumRooms = proplists:get_value(num_rooms, InitData),

	% build the initial room state of the monitor
	RoomState = lists:map(fun(Seq) ->
		% all rooms are in good condition at the beginning
		{Seq, condition_good}
	end, lists:seq(1, NumRooms)),

	io:format("fsm init at ~w~n", self()),

	{ok, monitoring, [{room_state, RoomState}]}.


% function callback for the state 'monitoring'
monitoring({event, Event}, StateData) ->
	% get the room's sequence number
	Seq = get_room_seq(Event),

	% get the room state that the event expresses
	NewState = get_event_state(Event),

	% get the previous state of the room
	PrevState = get_room_state(Seq, StateData),

	% update the room's state
	NewStateData = set_room_state(Seq, NewState, StateData),

	case NewState of
		% if the new state is the same as the previous state
		PrevState ->
			% no need for any reactions
			ok;

		% else
		_NotPrevState ->
			% react to this change of state
			change_room_state(Seq, PrevState, NewState)

	end,

	% continue monitoring
	{ok, monitoring, NewStateData}.


% extract the room sequence number from the event
get_room_seq(Event) ->
	% for now, just get the seq number for the node name
	Node = proplists:get_value(source, Event),

	% extract the number
	Parts = string:tokens(atom_to_list(Node), "@"),
	Name = hd(Parts),
	% the number is behind the '_' character
	MoreParts = string:tokens(Name, "_"),

	% return the sequence number as the last element of the name
	lists:last(MoreParts).


% extract the room condition state from the event
get_event_state(Event) ->
	proplists:get_value(type, Event).


% get the state of the room from the data
get_room_state(Seq, StateData) ->
	% get room states from the data
	RoomState = proplists:get_value(room_state, StateData),

	% get the state of the room represented by the sequence number
	proplists:get_value(Seq, RoomState).


% set the state of the room
set_room_state(Seq, NewState, StateData) ->
	% get room states from the data
	RoomState = proplists:get_value(room_state, StateData),

	% set the state of the specified room
	NewRoomState = [{Seq, NewState} | proplists:delete(Seq, RoomState)],

	% return the new state data
	[{room_state, NewRoomState} | proplists:delete(room_state, StateData)].


% payload for changing the room's state
change_room_state(Seq, PrevState, NewState) ->
	io:format("room_condition_master: room state changed from ~w to ~w~n", [PrevState, NewState]),
	% to be implemented
	ok.


% handle stop request
handle_event(stop, _StateName, StateData) ->
	% stop the fsm
	{stop, normal, StateData}.


% terminate callback function
terminate(_Reason, _StateName, _StateData) ->
	ok.


% code_change callback function
code_change(_OldSvn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.


% handle_info callback function
handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.


% handle_sync_event callback function
handle_sync_event(_Event, _From, StateName, StateData) ->
	{next_state, StateName, StateData}.
