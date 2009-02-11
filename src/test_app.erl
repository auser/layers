-module (test_app).
-compile (export_all).

% start_layers() ->
	% layers:init().
	% layers:add(converse, [{port, 1234}]).
	% layers:add(whisper, []).
	% layers:add(email, [{to_email, "alerner@att.com"}]).
	% layers:add(test_app, []).
	% layers:start().

start() ->
	converse:start(normal, [{layers_receive, [?MODULE, layers_receive]}]).

layers_receive(From) ->
	receive
		{data, Socket, Data} ->
			io:format("Received function ~p~n",[Data]),
			case Data of
				{data, Message} ->
					io:format("Received data ~p from ~p~n", [Message, From]),
					layers_receive(From);
				{who_are_you} ->
					io:format("Received who are you from ~p~n", [Socket]),
					layers_receive(From);
				Anything ->
					io:format("Received anything: ~p~n", [Anything]),
					layers_receive(From)
			end;
		Anything ->
			io:format("Received ~p~n", [Anything]),
			layers_receive(From)
	end.

hear(From) ->
	receive
		Anything ->
			io:format("Heard ~p~n", [From]),
			hear(From)
	end.