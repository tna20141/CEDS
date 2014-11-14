-module(room_condition).
-include("general.hrl").
-behavior(gen_fsm).

-export([start_link/1, init/1]).
-export([good/2, bad/2]).
-export([handle_info/3, handle_sync_event/4, handle_event/3, code_change/4, terminate/3]).

-define(PAYLOAD_SIZE, 1024).
-define(TEMPERATURE_LOW, 16).
-define(TEMPERATURE_HIGH, 25).
-define(HUMIDITY_LOW, 25).
-define(HUMIDITY_HIGH, 40).


% start the fsm
start_link(_Args) ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).


% initialize the fsm
init(_InitData) ->
	% build the init condition data
	State = [{temperature, good}, {humidity, good}],

	% start with being 'good'
	{ok, good, State}.


% function callback for the state 'good'
good({event, Event}, StateData) ->
	% analyze the event
	{Type, Condition} = analyze_condition_event(Event),

	% update the state data and see if we need to downgrade to 'bad'
	{Downgrade, NewStateData} = update_state(Type, Condition, StateData),

	case Downgrade of
		% need to downgrade
		true ->
			% create a new downgrade event
			GenEvent = utils:create_event(condition_bad, utils:create_data(?PAYLOAD_SIZE)),

			% send it
			forwarder:generate_event(GenEvent),

			% change state to 'bad'
			{next_state, bad, NewStateData};

		% else
		false ->
			% stay with being 'good'
			{next_state, good, NewStateData}

	end.


% function callback for the state 'bad'
bad({event, Event}, StateData) ->
	% analyze the event
	{Type, Condition} = analyze_condition_event(Event),

	% update the state data and see if we need to upgrade to 'good'
	{Upgrade, NewStateData} = update_state(Type, Condition, StateData),

	case Upgrade of
		% need to upgrade
		true ->
			% create a new upgrade event
			GenEvent = utils:create_event(condition_good, utils:create_data(?PAYLOAD_SIZE)),

			% send it
			forwarder:generate_event(GenEvent),

			% change state to 'good'
			{next_state, good, NewStateData};

		% else
		false ->
			% stay with being 'bad'
			{next_state, bad, NewStateData}

	end.


% analyze the event
analyze_condition_event(Event) ->
	% get event type
	Type = proplists:get_value(type, Event),

	% get event data
	Data = proplists:get_value(private_data, Event),

	case Type of
		% temperature event
		temperature ->
			% analyze the temperature value
			Result = analyze_temperature(Data),

			% return the results
			{Type, Result};

		% humidity event
		humidity ->
			% analyze the humidity value
			Result = analyze_humidity(Data),

			% return the results
			{Type, Result}

	end.


% update the state according to the analization results
update_state(Type, Condition, StateData) ->
	% get the room status before update
	Before = get_status(StateData),

	% update the state
	NewState = [{Type, Condition} | proplists:delete(Type, StateData)],

	% get the room status after the update
	After = get_status(StateData),

	Change = case Before of
		% if before is the same as after
		After ->
			% no need to change fsm state
			false;

		% else
		_NotAfter ->
			% fsm needs to change state
			true

	end,

	{Change, NewState}.


% get the room condition status (good or bad)
get_status(StateData) ->
	% get the status attributes
	TemperatureStatus = proplists:get_value(temperature, StateData),
	HumidityStatus = proplists:get_value(humidity, StateData),

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
analyze_temperature(Data) ->
	% extract the temperature value
	<<Temperature/integer, _Padding/binary>> = Data,

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
analyze_humidity(Data) ->
	% extract the humidity value
	<<Humidity/integer, _Padding>> = Data,

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
