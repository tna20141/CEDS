-module(er_sup).
-include("general.hrl").
-behavior(supervisor).

-export([init/1]).
-export([start_link/0]).


% start the supervisor
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


% initialize the supervisor from the configuration file
init(_InitData) ->
	% initialize the necessary data structures
	TableIds = init_tables(),

	% build the children specification
	ChildrenSpec = make_children_spec(TableIds),

	% return the results
	{ok, {{one_for_one, 1, 1}, ChildrenSpec}}.


% initialize the tables needed for operations
init_tables() ->
	% init the local subscription table
	SubTable = ets:new(sub_table, [set, public]),

	% create a mutex for the subscription table
	SubMutex = mutex:new(),

	% return the table ids
	[
		{sub_table, SubTable},
		{sub_mutex, SubMutex}
	].


% build the children specification
make_children_spec(TableIds) ->
	% build the base loop data
	BaseLoopData = make_base_loop_data(TableIds),

	% return the children specification
	[
		{?SUB_ACCEPTOR, {sub_acceptor, start_link, [BaseLoopData]},
				permanent, 2000, worker, [sub_acceptor]},
		{?ROUTER, {router, start_link, [BaseLoopData]},
				permanent, 2000, worker, [router]}
	].


% initialize the loop data used by the persistent services
make_base_loop_data(TableIds) ->
	% get the subscription table id
	SubTable = proplists:get_value(sub_table, TableIds),

	% get the subscription table mutex
	SubMutex = proplists:get_value(sub_mutex, TableIds),

	% build and return the base loop data
	[
		{sub_table, SubTable},
		{sub_mutex, SubMutex}
	].
