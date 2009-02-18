-module (layers_test_app).
-compile (export_all).

start_layers() ->
	layers:init(),
	layers:add(converse, [{port, 1234}]),
	layers:add(layers_test_app, []),
	layers:start().

start_slim() ->
	layers:start([converse, layers_test_app], [{port, 1234}]).

start(_Type, Config) ->
	io:format("Starting layers_test_app with ~p~n", [Config]).
	
test() ->
	converse:open_and_send({10,45,10,234}, {data, whisper:encrypt("hi")}).

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
			layers_receive(From)
	end.