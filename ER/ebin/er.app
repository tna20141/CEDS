{
	application, er, [
		{description, "Event Router"},
		{vsn, "1.0"},
		{modules, [sub_acceptor, router, er_sup, er_app]},
		{registered, [sub_acceptor, router]},
		{applications, [kernel, stdlib]},
		{env, []},
		{mod, {er_app, []}}
	]
}.