-module(mutex).
-export([new/0, wait/1, signal/1, destroy/1, init/0]).

% initialize the mutex
init() ->
	% a newly created mutex is 'free'
	free().


% implement the 'free' state of the mutex
free() ->
	receive
		% a wait request from a process
		{wait, Pid} ->
			% give the process the pass
			Pid ! {ok, self()},

			% change state to 'busy' on the process
			busy(Pid);

		% the mutex is being destroyed
		stop ->
			% clean up
			terminate()
	end.


% implement the 'busy' state of the mutex
busy(Pid) ->
	receive
		% the holding process releases the mutex
		{signal, Pid} ->
			% change back to being 'free'
			free();

		% the mutex is being destroyed
		stop ->
			% clean up
			terminate()
	end.


% clean up function
terminate() ->
	% kill all the waiting processes
	receive
		{wait, Pid} ->
			exit(Pid, kill),
			% continue killing...
			terminate()

	% ... until there's no waiting process left
	after
		0 ->
			ok
	end.


%%
%% API
%%

% wait on the mutex
wait(Mutex) ->
	% send the mutex a wait request
	Mutex ! {wait, self()},

	% wait for the reply
	receive
		{ok, Mutex} ->
			ok
	end.


% signal the mutex
signal(Mutex) ->
	% send the mutex a signal message
	Mutex ! {signal, self()},
	ok.


% allocate a new mutex
new() ->
	% the mutex is implemented as a process waiting for messages
	spawn_link(?MODULE, init, []).


% destroy the mutex
destroy(Mutex) ->
	% send the mutex process a stop message
	Mutex ! stop,
	ok.
