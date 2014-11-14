{
	application, ed, [
		{description, "Event Detector"},
		{vsn, "1.0"},
		{modules, [local_proxy, forwarder, ed_sup, ed_app]},
		{registered, [local_proxy, forwarder]},
		{applications, [kernel, stdlib]},
		{env, []},
		{mod, {ed_app, [{config_file, "./priv/ed.conf"}]}}
	]
}.