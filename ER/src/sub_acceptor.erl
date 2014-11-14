-module(sub_acceptor).
-include("general.hrl").
-behavior(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([start_link/1, debug/0]).


% start the subscription acceptor server
start_link(BaseLoopData) ->
	gen_server:start_link({local, ?SUB_ACCEPTOR}, ?MODULE, BaseLoopData, []).


% initialize the subscription acceptor
init(BaseLoopData) ->
	{ok, BaseLoopData}.


% handle register messages from EDs
handle_call({register, Register}, _From, LoopData) ->
	io:format("sub_acceptor: register message from ~w~n", [_From]),

	% get the ed table
	EDTable = proplists:get_value(ed_table, LoopData),

	% get the ed node name of the registration
	Node = proplists:get_value(node, Register),

	% get the node info of the registration
	NodeInfo = get_node_info(Register),

	% insert the ED into the table
	ets:insert(EDTable, {Node, NodeInfo}),

	{reply, ok, LoopData};


% handle deregister messages from EDs
handle_call({deregister, Deregister}, _From, LoopData) ->
	io:format("sub_acceptor: deregister message from ~w~n", [_From]),

	% get the ed table
	EDTable = proplists:get_value(ed_table, LoopData),

	% get the ed node name of the registration
	Node = proplists:get_value(node, Deregister),

	% remove the node from the table
	ets:delete(EDTable, Node),

	{reply, ok, LoopData};


% handle subscription messages from EDs
handle_call({subscribe, Subscription}, _From, LoopData) ->
	io:format("sub_acceptor: subscribe message from ~w~n", [_From]),

	% insert the subscription to the subscription table
	insert_sub_to_table(Subscription, LoopData),

	{reply, ok, LoopData};


% handle unsubscription messages from EDs
handle_call({unsubscribe, Unsubscription}, _From, LoopData) ->
	io:format("sub_acceptor: unsubscribe message from ~w~n", [_From]),

	% remove the subscriptions from the subscription table
	remove_sub_from_table(Unsubscription, LoopData),

	{reply, ok, LoopData};


% handle debug message from the console
handle_call(debug, _From, LoopData) ->
	% print out loopdata
	io:format("sub_acceptor: loopdata: ~w~n", [LoopData]),

	% print out subscription table
	SubTable = proplists:get_value(sub_table, LoopData),

	io:format("sub_acceptor: sub_table: ~n"),
	ets:foldl(fun(Entry, _AccIn) ->
		io:format("\t~w~n", [Entry])
	end, [], SubTable),

	% print out ed table
	EDTable = proplists:get_value(ed_table, LoopData),

	io:format("sub_acceptor: ed_table: ~n"),
	ets:foldl(fun(Entry, _AccIn) ->
		io:format("\t~w~n", [Entry])
	end, [], EDTable),

	{reply, ok, LoopData}.


% handle stop messages
handle_cast(stop, LoopData) ->
	io:format("sub_acceptor: stop handler called~n"),
	{stop, normal, LoopData}.


% termination cleanup callback
terminate(_Reason, _LoopData) ->
	ok.


% API for debugging from the console
debug() ->
	gen_server:call(?SUB_ACCEPTOR, debug).


% insert the subscription to the subscription table
insert_sub_to_table(Subscription, LoopData) ->
	% get the subscription table
	SubTable = proplists:get_value(sub_table, LoopData),

	% get the subscription table mutex
	SubMutex = proplists:get_value(sub_mutex, LoopData),

	% get the subscription event type
	Type = proplists:get_value(type, Subscription),

	% get the fsm id
	FsmId = proplists:get_value(fsm_id, Subscription),

	% wait mutex
	mutex:wait(SubMutex),

	% get the entry associated with the event type in the table
	Entry = ets:lookup(SubTable, Type),

	case Entry of
		% there hasn't been any subscription on this type yet
		[] ->
			% put the new subscription in
			ets:insert(SubTable, {Type, [[{fsm_id, FsmId}]]});

		% else, there are already subscriptions on the type
		[{Type, OldList}] ->
			% put the new subscription into the list
			ets:insert(SubTable, {Type, [[{fsm_id, FsmId}] | OldList]})

	end,

	mutex:signal(SubMutex),

	ok.


% remove the subscriptions from the subscription table
remove_sub_from_table(Unsubscription, LoopData) ->
	% get the subscription table
	SubTable = proplists:get_value(sub_table, LoopData),

	% get the subscription table mutex
	SubMutex = proplists:get_value(sub_mutex, LoopData),

	% get the fsm id
	FsmId = proplists:get_value(fsm_id, Unsubscription),

	% wait mutex
	mutex:wait(SubMutex),

	% get the first entry of the table
	First = ets:first(SubTable),

	case First of
		% if the table is empty
		'$end_of_table' ->
			% nothing to do, release the mutex
			mutex:signal(SubMutex),
			ok;

		% else
		{Type, SubList} ->
			% cleanse the entry of the fsm id
			NewSubList = remove_subs_with_fsm_id(FsmId, SubList),

			case NewSubList of
				% if the result list is empty
				[] ->
					% no need for an entry for the type
					ets:delete(SubTable, Type);

				% else
				_NotEmpty ->
					% put the new list back into the table
					ets:insert(SubTable, {Type, NewSubList})

			end,

			% signal mutex
			mutex:signal(SubMutex),

			case NewSubList of
				% the new sub list is the same as the old list
				SubList ->
					ok;

				% if the new list is different
				_NotIdentical ->
					% send a flush message to the related EDs
					flush_all_eds(Type, LoopData)

			end,

			% continue with the next entry
			remove_sub_from_table_next(First, Unsubscription, LoopData)

	end.


remove_sub_from_table_next(Entry, Subscription, LoopData) ->
	% get the subscription table
	SubTable = proplists:get_value(sub_table, LoopData),

	% get the subscription table mutex
	SubMutex = proplists:get_value(sub_mutex, LoopData),

	% get the fsm id
	FsmId = proplists:get_value(fsm_id, Subscription),

	% wait mutex
	mutex:wait(SubMutex),

	% get the next entry of the table
	Next = ets:next(SubTable, Entry),

	case Next of
		% Entry is already the alst one
		'$end_of_table' ->
			% nothing to do, release the mutex
			mutex:signal(SubMutex),
			ok;

		% else
		{Type, SubList} ->
			% cleanse the entry of the fsm id
			NewSubList = remove_subs_with_fsm_id(FsmId, SubList),

			case NewSubList of
				% if the result list is empty
				[] ->
					% no need for an entry for the type
					ets:delete(SubTable, Type);

				% else
				_NotEmpty ->
					% put the new list back into the table
					ets:insert(SubTable, {Type, NewSubList})

			end,

			% signal mutex
			mutex:signal(SubMutex),

			case NewSubList of
				% the new sub list is the same as the old list
				SubList ->
					ok;

				% if the new list is different
				_NotIdentical ->
					% send a flush message to the EDs
					flush_all_eds(Type, LoopData)

			end,

			% continue with the next entry
			remove_sub_from_table_next(Next, Subscription, LoopData)

	end.


% remove all subscriptions of the fsm from the list
remove_subs_with_fsm_id(FsmId, SubList) ->
	lists:foldl(fun(Sub, Acc) ->
		% get the fsm id of the subscription
		SubFsmId = proplists:get_value(fsm_id, Sub),

		case SubFsmId of
			% if it is identical to the fsm id of the unsubscription
			FsmId ->
				% remove (dont add it to the new list)
				Acc;

			% if it is not, leave it in the new list
			_NotIdentical ->
				[Sub | Acc]

		end

	end, [], SubList).


% send a flush message to all EDs
flush_all_eds(Type, LoopData) ->
	% get the ED table
	EDTable = proplists:get_value(ed_table, LoopData),

	% get the first entry
	First = ets:first(EDTable),

	case First of
		% the table is empty
		'$end_of_table' ->
			% nothing to do
			ok;

		% else
		{Node, _NodeInfo} ->
			% send the flush message to the node's forwarder
			gen_server:call({?FORWARDER, Node}, {flush, [{type, Type}]}),

			% continue
			flush_all_eds_next(First, Type, LoopData)

	end.


flush_all_eds_next(Entry, Type, LoopData) ->
	% get the ED table
	EDTable = proplists:get_value(ed_table, LoopData),

	% get the next entry
	Next = ets:next(EDTable, Entry),

	case Next of
		% Entry is already the last one
		'$end_of_table' ->
			% nothing to do
			ok;

		% else
		{Node, _NodeInfo} ->
			% send the flush message to the node's forwarder
			gen_server:call({?FORWARDER, Node}, {flush, [{type, Type}]}),

			% continue
			flush_all_eds_next(Next, Type, LoopData)

	end.


% extract the node info from the registration
get_node_info(_Register) ->
	% for now, just return an empty list
	[].


% code_change callback function
code_change(_OldVsn, _State, _Extra) ->
	{ok, no_state}.


% handle_info callback function
handle_info(_Info, _State) ->
	{noreply, no_state}.
