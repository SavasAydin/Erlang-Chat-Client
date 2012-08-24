{application, chat,
	[{description, "Welcome to funky torrent chat system"},
	{vsn, "1.2"},
	{modules, [chat_app, chat_supervisor, message_server, db_server]},
	{registered, [message_server]},
	{applications, [kernel, stdlib]},
	{mod, {chat_app, []}},
	{start_phases, []}
	]}.
