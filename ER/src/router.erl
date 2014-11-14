-module(router).
-include("general.hrl").
-behavior(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([start_link/1]).


% start the subscription acceptor server
start_link(BaseLoopData) ->
	gen_server:start_link({local, ?ROUTER}, ?MODULE, BaseLoopData, []).


% initialize the subscription acceptor
init(BaseLoopData) ->
	{ok, BaseLoopData}.


% handle route query from the ED's forwarder
handle_call({ask_route, Event}, _From, LoopData) ->
	% get the event type
	Type = proplists:get_value(type, Event),

	% get the subscription table to lookup
	SubTable = proplists:get_value(sub_table, LoopData),

	% get the entry associated with the event type from the subscription table
	Entry = ets:lookup(SubTable, Type),

	SubList = case Entry of
		% no route for this type
		[] ->
			[];

		% there are sinks for this type
		[{Type, List}] ->
			List

	end,

	% extract the sink list from the subscriptions
	SinkList = extract_sinks(SubList),

	% return the results
	{reply, {ok, [{sink_list, SinkList}]}, LoopData}.


% handle stop messages
handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.


% termination cleanup callback
terminate(_Reason, _LoopData) ->
	ok.


% extract the sink list from the subscriptions
extract_sinks(SubList) ->
	lists:map(fun(Sub) ->

		% get the fsm id from the subscription
		FsmId = proplists:get_value(fsm_id, Sub),

		% return the node name as the sink
		{_FsmNumber, Node} = FsmId,
		Node

	end, SubList).


% code_change callback function
code_change(_OldVsn, _State, _Extra) ->
	{ok, no_state}.


% handle_info callback function
handle_info(_Info, _State) ->
	{noreply, no_state}.
