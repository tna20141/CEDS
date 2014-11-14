-module(forwarder).
-include("general.hrl").
-behavior(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([start_link/1, generate_event/1]).


% start the forwarder server
start_link(BaseLoopData) ->
	gen_server:start_link({local, ?FORWARDER}, ?MODULE, BaseLoopData, []).


% initialize the forwarder
init(BaseLoopData) ->
	{ok, BaseLoopData}.


% handle event messages from the fsms
handle_call({event, Event}, _From, LoopData) ->
	% find the event sinks to forward the event to
	SinkList = find_sinks(Event, LoopData),

	% forward the event to each sink
	lists:foreach(fun(Sink) ->

		forward_event(Event, Sink)

	end, SinkList),

	{reply, ok, LoopData};


% handle route flush messages from the ER's router
handle_call({flush, FlushInfo}, _From, LoopData) ->
	io:format("forwarder: flush message from ~w~n", [_From]),

	% get the local routing table
	RoutingTable = proplists:get_value(routing_table, LoopData),

	% get the event type to flush
	Type = proplists:get_value(type, FlushInfo),

	% flush
	ets:delete(RoutingTable, Type),

	{reply, ok, LoopData}.


% handle stop messages
handle_cast(stop, LoopData) ->
	io:format("forwarder: stop handler called~n"),
	{stop, normal, LoopData}.


% termination cleanup callback
terminate(_Reason, LoopData) ->
	ok.


% find all the event sinks for a certain event recently generated
find_sinks(Event, LoopData) ->
	% get the event type to lookup
	Type = proplists:get_value(type, Event),

	% get the local routing table
	RoutingTable = proplists:get_value(routing_table, LoopData),

	% lookup from the local table
	SinkList = ets:lookup(RoutingTable, Type),

	FinalSinkList = case SinkList of
		% if local lookup is empty, we have to consult the ER
		[] ->
			% get the ER's router destination
			Router = proplists:get_value(router, LoopData),

			% send the query
			{ok, Result} = gen_server:call(Router, {ask_route, Event}),

			% extract the sink list
			NewSinkList = proplists:get_value(sink_list, Result),

			case NewSinkList of
				% if the returned sink list is empty, no need to update the local table
				[] ->
					[];

				% otherwise, update the local table
				_NotEmpty ->
					ets:insert(RoutingTable, {Type, NewSinkList}),
					NewSinkList

			end;

		% if local lookup is not empty, just use it
		_LocalNotEmpty ->
			SinkList

	end,

	FinalSinkList.


% forward an event to a certain sink
forward_event(Event, Sink) ->
	% forward to the sink's local proxy
	gen_server:call({?LOCAL_PROXY, Sink}, {event, Event}),

	ok.


% this function acts as an API call for the fsm's code to generate a new event
generate_event(Event) ->
	% send the event to the forwarder
	gen_server:call(?FORWARDER, {event, Event}).


% code_change callback function
code_change(_OldVsn, _State, _Extra) ->
	{ok, no_state}.


% handle_info callback function
handle_info(_Info, _State) ->
	{noreply, no_state}.
