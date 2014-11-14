-module(local_proxy).
-include("general.hrl").
-behavior(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([start_link/2, debug/0]).


% start the local proxy server
start_link(BaseLoopData, SubscriptionList) ->
	gen_server:start_link({local, ?LOCAL_PROXY}, ?MODULE, {BaseLoopData, SubscriptionList}, []).


% initialize the local proxy server
init({BaseLoopData, SubscriptionList}) ->
	% start the fsms and subscribe to the local proxy in a separate process
	erlang:spawn(fun() ->
		init_fsms(SubscriptionList)
	end),

	% register with the ER
	SubAcceptor = proplists:get_value(sub_acceptor, BaseLoopData),
	register_to_er(SubAcceptor),

	{ok, [{fsm_num, 0} | BaseLoopData]}.


% handle fsms' subscribe messages
handle_call({subscribe, Subscription}, _From, LoopData) ->
	io:format("local_proxy: subscribe message from ~w~n", [_From]),

	% allocate the next fsm number
	FsmNumber = next_id(LoopData),

	% get the fsm specification
	Fsm = proplists:get_value(fsm_spec, Subscription),

	% register the fsm
	Result = register_fsm(FsmNumber, Fsm, LoopData),

	% check the result
	case Result of
		% success
		ok ->
			% update the fsm number status
			NewLoopData = [{fsm_num, FsmNumber+1} | proplists:delete(fsm_num, LoopData)],

			% get the subscription acceptor info
			SubAcceptor = proplists:get_value(sub_acceptor, LoopData),

			% process subscription list for the fsm
			Subs = proplists:get_value(sub_list, Subscription),

			lists:foreach(fun(Sub) ->

				% get the event type of the subscription
				Type = proplists:get_value(type, Sub),

				% is this event local or remote
				From = proplists:get_value(from, Sub),

				case From of
					% the event type is local, no need to subscribe to the ER
					local ->
						ok;

					% the event is remote
					remote ->
						% subscribe to the ER
						gen_server:call(SubAcceptor,
								{subscribe, [{type, Type}, {fsm_id, fsm_id(FsmNumber)}]})

				end,

				% insert the subscription to the local subscription table
				insert_sub_to_table(Type, FsmNumber, LoopData)

			end, Subs),

			% return the fsm number to the caller
			{reply, {ok, FsmNumber}, NewLoopData};

		% failure
		{error, cannot_start_fsm} ->
			{reply, {error, cannot_start_fsm}, LoopData}

	end;


% handle fsms' unsubscribe messages
% the local removal of the fsm's subscription is not done here and will be done lazily
handle_call({unsubscribe, Unsubscription}, _From, LoopData) ->
	io:format("local_proxy: unsubscribe message from ~w~n", [_From]),

	% get fsm number
	FsmNumber = proplists:get_value(fsm_num, Unsubscription),

	% get the subscription acceptor info
	SubAcceptor = proplists:get_value(sub_acceptor, LoopData),

	% unsubscribe from the ER
	gen_server:call(SubAcceptor,
			{unsubscribe, [{fsm_id, fsm_id(FsmNumber)}]}),

	% remove the fsm from the registered fsm list
	deregister_fsm(FsmNumber, LoopData),

	{reply, ok, LoopData};


% handle event messages coming from other event sources
handle_call({event, Event}, _From, LoopData) ->
	% get the event type
	Type = proplists:get_value(type, Event),

	% get the fsm list that subscribed to the event
	FsmList = get_subscribed_fsms(Type, LoopData),

	% deliver the event to each fsm
	lists:foreach(fun(FsmNumber) ->

		deliver_event(Event, FsmNumber, LoopData)

	end, FsmList),

	{reply, ok, LoopData};


% handle debug message from the console
handle_call(debug, _From, LoopData) ->
	% print out loopdata
	io:format("local_proxy: loopdata: ~w~n", [LoopData]),

	% print out subcription table
	SubTable = proplists:get_value(sub_table, LoopData),

	io:format("local_proxy: sub_table: ~n"),
	ets:foldl(fun(Entry, _AccIn) ->
		io:format("\t~w~n", [Entry])
	end, [], SubTable),

	{reply, ok, LoopData}.


% handle stop messages
handle_cast(stop, LoopData) ->
	io:format("local_proxy: stop handler called~n"),

	% get the fsm list
	FsmTable = proplists:get_value(fsm_table, LoopData),

	% shutdown all fsms
	shutdown_all(FsmTable),

	% deregister from the er
	SubAcceptor = proplists:get_value(sub_acceptor, LoopData),
	deregister_from_er(SubAcceptor),

	{stop, normal, LoopData}.


% termination cleanup callback
terminate(_Reason, _LoopData) ->
	ok.


% API for debugging from the console
debug() ->
	gen_server:call(?LOCAL_PROXY, debug).


% shutdown all fsms running on this node
shutdown_all(FsmTable) ->
	% check the first entry of the table
	First = ets:first(FsmTable),

	case First of
		% if there is nothing on the table
		'$end_of_table' ->
			% nothing to do
			ok;

		% else
		{_FsmNumber, Pid} ->
			% stop the fsm
			gen_fsm:send_all_state_event(Pid, stop),

			% continue with the next fsm
			shutdown_next(First, FsmTable)

	end.


shutdown_next(Entry, FsmTable) ->
	% get the next fsm to shut down
	Next = ets:next(FsmTable, Entry),

	case Next of
		% Entry is already the alst one
		'$end_of_table' ->
			% nothing to do
			ok;

		% else, continue
		{_FsmNumber, Pid} ->
			gen_fsm:send_all_state_event(Pid, stop),
			shutdown_next(Next, FsmTable)

	end.


% allocate the next fsm number
next_id(LoopData) ->
	% just return the next fsm number
	proplists:get_value(fsm_num, LoopData).


% register and start an fsm
register_fsm(FsmNumber, Fsm, LoopData) ->
	{Module, Arguments} = Fsm,

	% start the fsm
	Result = Module:start_link(Arguments),

	% check the result
	case Result of
		% success
		{ok, Pid} ->
			% get the fsm table
			FsmTable = proplists:get_value(fsm_table, LoopData),

			% insert the fsm into the table
			ets:insert(FsmTable, {FsmNumber, Pid}),
			ok;

		% failure
		_Error ->
			{error, cannot_start_fsm}

	end.


% deregister the fsm from the node
deregister_fsm(FsmNumber, LoopData) ->
	% get fsm table
	FsmTable = proplists:get_value(fsm_table, LoopData),

	% remove the fsm into the table
	ets:delete(FsmTable, FsmNumber),

	ok.


% insert a subscription to the local subscription table
insert_sub_to_table(Type, FsmNumber, LoopData) ->
	% get subscription table
	SubTable = proplists:get_value(sub_table, LoopData),

	% get the subscription table mutex
	SubMutex = proplists:get_value(sub_mutex, LoopData),

	% wait mutex
	mutex:wait(SubMutex),

	% get entry for the specified type
	Entry = ets:lookup(SubTable, Type),

	case Entry of
		% there is no subscription for this type yet
		[] ->
			ets:insert(SubTable, {Type, [FsmNumber]});

		% if there are already fsms subscribed, add the new one to the list
		[{Type, FsmList}] ->
			ets:insert(SubTable, {Type, [FsmNumber | FsmList]})

	end,

	% signal mutex
	mutex:signal(SubMutex),

	ok.


% remove a subscription to a certain event of an fsm from the local subscription table
remove_sub_from_table(Event, FsmNumber, LoopData) ->
	% get subscription table
	SubTable = proplists:get_value(sub_table, LoopData),

	% get the event type
	Type = proplists:get_value(type, Event),

	% get the subscription table mutex
	SubMutex = proplists:get_value(sub_mutex, LoopData),

	% wait mutex
	mutex:wait(SubMutex),

	% get entry for the specified type
	Entry = ets:lookup(SubTable, Type),

	case Entry of
		% there is no subscription for this type yet, so nothing to remove
		[] ->
			no_subscription;

		% remove the fsm number from the list
		[{Type, FsmList}] ->
			NewFsmList = lists:delete(FsmNumber, FsmList),

			case NewFsmList of
				% if the new list is empty, there's no need for an entry in the table
				[] ->
					ets:delete(SubTable, Type);

				% if the new list is not empty, update to it
				_NotEmpty ->
					ets:insert(SubTable, {Type, NewFsmList})

			end

	end,

	% signal mutex
	mutex:signal(SubMutex),

	ok.


% get the list of fsms that subscribed to a certain type of event
get_subscribed_fsms(Type, LoopData) ->
	% get subscription table
	SubTable = proplists:get_value(sub_table, LoopData),

	% get entry for the specified type
	Entry = ets:lookup(SubTable, Type),

	SubList = case Entry of
		% there is no subscription for this type yet
		[] ->
			[];

		% there are fsms subscribed
		[{Type, FsmList}] ->
			FsmList

	end,

	SubList.


% deliver the event to the specified fsm
deliver_event(Event, FsmNumber, LoopData) ->
	% get fsm table
	FsmTable = proplists:get_value(fsm_table, LoopData),

	% get the fsm to send the event to
	Entry = ets:lookup(FsmTable, FsmNumber),

	case Entry of
		% if the fsm doesn't exist, remove it from the subscribe table
		[] ->
			remove_sub_from_table(Event, FsmNumber, LoopData);

		% send the event
		[{FsmNumber, Fsm}] ->
			gen_fsm:send_event(Fsm, {event, Event}),

			ok

	end.


% initialize the fsms from the list of subscriptions
init_fsms(SubscriptionList) ->
	% sleep for a while to ensure the the local proxy is fully started
	timer:sleep(10),

	% process each subscription
	lists:foreach(fun(Subscription) ->

		% send the subscription to the local proxy
		Result = gen_server:call(?LOCAL_PROXY, {subscribe, Subscription}),

		% check the returned value
		case Result of
			% success
			{ok, _FsmNumber} ->
				ok;

			% failure
			{error, _Reason} ->
				% for now, we do nothing
				nothing

		end

	end, SubscriptionList).


% return the fsm id as seen by the ER
fsm_id(FsmNumber) ->
	% return the combination of fsm number and the node name
	{FsmNumber, node()}.


% send a register message to the ER
register_to_er(SubAcceptor) ->
	% build the message
	Message = {register, [{node, node()}]},

	% send it to the sub acceptor
	gen_server:call(SubAcceptor, Message),

	ok.


% send a deregister message to the ER
deregister_from_er(SubAcceptor) ->
	% build the message
	Message = {deregister, [{node, node()}]},

	% send it to the sub acceptor
	gen_server:call(SubAcceptor, Message),

	ok.


% code_change callback function
code_change(_OldVsn, _State, _Extra) ->
	{ok, no_state}.


% handle_info callback function
handle_info(_Info, _State) ->
	{noreply, no_state}.
