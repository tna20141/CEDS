-module(ed_sup).
-include("general.hrl").
-behavior(supervisor).

-export([init/1]).
-export([start_link/1]).


% start the supervisor
start_link(ConfigFile) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, ConfigFile).


% initialize the supervisor from the configuration file
init(ConfigFile) ->
	% load configuration file
	Configuration = file:consult(ConfigFile),

	case Configuration of
		% configuration file read succeeded
		{ok, Terms} ->
			FinalConfiguation = hd(Terms),

			% initialize the necessary data structures
			TableIds = init_tables(),

			% process the configuration
			ChildrenSpec = handle_configuration(FinalConfiguation, TableIds),

			% return the results
			{ok, {{one_for_one, 1, 1}, ChildrenSpec}};

		% configuration file read failed
		{error, Reason} ->
			{error, Reason}

	end.


% parse the configuration file
handle_configuration(Configuation, TableIds) ->
	% build the base loop data for the local services
	BaseLoopData = make_base_loop_data(Configuation, TableIds),

	% get the fsm configuration list
	FsmList = proplists:get_value(fsm_list, Configuation),

	% get the subscriptions of each fsm
	SubscriptionList = lists:map(fun(Fsm) ->

		% get fsm spec
		{Module, Arguments} = proplists:get_value(fsm_spec, Fsm),

		% get subscription list
		SubList = proplists:get_value(sub_list, Fsm),

		% process it
		ProcessedSubList = lists:map(fun(Sub) ->

			% get the event type of the subscription
			Type = proplists:get_value(type, Sub),

			% get the event origins
			From = proplists:get_value(from, Sub),

			[{type, Type}, {from, From}]

		end, SubList),

		% build the subscription message
		[{fsm_spec, {Module, Arguments}}, {sub_list, ProcessedSubList}]

	end, FsmList),

	% build and return the children specification
	[
		{?LOCAL_PROXY, {local_proxy, start_link, [BaseLoopData, SubscriptionList]},
				permanent, 2000, worker, [local_proxy]},
		{?FORWARDER, {forwarder, start_link, [BaseLoopData]},
				permanent, 2000, worker, [forwarder]}
	].


% initialize the tables needed for operations
init_tables() ->
	% init the local subscription table
	SubTable = ets:new(sub_table, [set, public]),

	% init the local fsm table
	FsmTable = ets:new(fsm_table, [set, public]),

	% init the local routing table
	RoutingTable = ets:new(routing_table, [set, public]),

	% create a mutex for the subscription table
	SubMutex = mutex:new(),

	% return the table ids
	[
		{sub_table, SubTable},
		{fsm_table, FsmTable},
		{routing_table, RoutingTable},
		{sub_mutex, SubMutex}
	].


% initialize the loop data used by the persistent services
make_base_loop_data(Terms, TableIds) ->
	% get the subscription table id
	SubTable = proplists:get_value(sub_table, TableIds),

	% get the fsm table id
	FsmTable = proplists:get_value(fsm_table, TableIds),

	% get the routing table id
	RoutingTable = proplists:get_value(routing_table, TableIds),

	% get the subscription table mutex
	SubMutex = proplists:get_value(sub_mutex, TableIds),

	% get the ER node name
	ERNode = proplists:get_value(er_node, Terms),

	% build and return the base loop data
	[
		{sub_table, SubTable},
		{fsm_table, FsmTable},
		{routing_table, RoutingTable},
		{sub_mutex, SubMutex},
		{sub_acceptor, {?SUB_ACCEPTOR, ERNode}},
		{router, {?ROUTER, ERNode}}
	].
