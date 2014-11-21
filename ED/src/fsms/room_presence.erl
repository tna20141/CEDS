-module(room_presence).
-include("general.hrl").
-behavior(gen_fsm).

-export([start_link/1, init/1]).
-export([positive/2, negative/2]).
-export([handle_info/3, handle_sync_event/4, handle_event/3, code_change/4, terminate/3]).

-define(POSITIVE_TIMEOUT, 4).
-define(PAYLOAD_SIZE, 1024).


% start the fsm
start_link(_Args) ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).


% initialize the fsm
init(_InitData) ->
	% build the init presence data
	State = [{movement, negative}, {presence, negative}, {last_update, calendar:local_time()}],

	io:format("fsm init at ~w~n", [self()]),

	% start with negative (no people in the room)
	{ok, negative, State}.


% function callback for the state 'positive'
positive({event, Event}, StateData) ->
	% get the event type
	Type = analyze_presence_event(Event),

	case Type of
		% event implies that there are movements in the room
		movement_positive ->
			% nothing new, just continue monitoring
			{next_state, positive, StateData};

		% event implies that there are no movements in the room
		movement_negative ->
			% update the presence state
			NewStateData1 = [{movement, negative} | proplists:delete(movement, StateData)],

			% update time
			NewStateData2 = [{last_update, calendar:local_time()} |
					proplists:delete(last_update, NewStateData1)],

			% change state to negative
			{next_state, negative, NewStateData2}

	end.


% function callback for the state 'negative'
negative({event, Event}, StateData) ->
	% get the event type
	Type = analyze_presence_event(Event),

	% get the room presence state
	Presence = proplists:get_value(presence, StateData),

	case Type of
		% event implies that there are movements in the room
		movement_positive ->
			% change movement state back to positive
			NewStateData1 = [{movement, positive} | proplists:delete(movement, StateData)],

			% update time
			NewStateData2 = [{last_update, calendar:local_time()} |
					proplists:delete(last_update, NewStateData1)],

			case Presence of
				% if the presence state is currently negative
				negative ->
					% change it back to positive
					NewStateData3 = [{presence, positive} | proplists:delete(presence, NewStateData2)],

					% create a presence state change event
					GenEvent = utils:create_event(presence_positive, utils:create_data(?PAYLOAD_SIZE)),

					% send it
					forwarder:generate_event(GenEvent),

					% change state to positive
					{next_state, positive, NewStateData3};

				% if the presence state is currently positive
				positive ->
					% nothing else needed to be done
					{next_state, positive, NewStateData2}

			end;

		% event implies that there are no movements in the room
		movement_negative ->
			case Presence of
				% if the presence state is already negative
				negative ->
					% the negative event is already generated, just continue
					{next_state, negative, StateData};

				% if the presence state is positive
				positive ->
					% check the last update time to see the how long we've been negative
					LastUpdateTime = proplists:get_value(last_update, StateData),
					TimeDiff = get_timediff_in_seconds(LastUpdateTime, calendar:local_time()),

					% compare with the positive timeout value
					case TimeDiff >= ?POSITIVE_TIMEOUT of
						% if in negative long enough
						true ->
							% change presence state to negative
							NewStateData4 = [{presence, negative} | proplists:delete(presence, StateData)],

							% create a presence state change event
							GenEvent = utils:create_event(presence_negative, utils:create_data(?PAYLOAD_SIZE)),

							% send it
							forwarder:generate_event(GenEvent),

							% next state is still negative
							{next_state, negative, NewStateData4};

						% not long enough
						false ->
							% continue monitoring
							{next_state, negative, StateData}

					end

			end

	end.


% analyze the presence event
analyze_presence_event(Event) ->
	proplists:get_value(type, Event).


% get the time difference between 2 datetime structures in seconds
get_timediff_in_seconds(TimeBefore, TimeAfter) ->
	% get time difference
	{Days, Time} = calendar:time_difference(TimeBefore, TimeAfter),

	% convert it to number of seconds
	Seconds = calendar:time_to_seconds(Time),
	86400*Days+Seconds.


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
