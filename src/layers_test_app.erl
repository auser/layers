-module (layers_test_app).
-compile (export_all).
-behaviour (application).

-define (PORT, 8765).

start_layers() ->
	layers:init(),
	layers:add(converse, [{port, ?PORT}]),
	% layers:add(whisper, []),
	layers:add(layers_test_app, []),
	layers:start().

start_slim() ->
	layers:start([converse, whisper, layers_test_app], [{port, ?PORT}]),
	whisper:start(normal, [{successor, [whisper]}]).
	% converse:start(normal, [{port, 8765}, {successor, [whisper]}]).

start(_Type, Config) ->
	io:format("Starting layers_test_app with ~p~n", [Config]).
	% ,Self = self(), Fun = config:parse(successor, Config)
	% ,layers:register_process(Fun, Self).
	
init([Config]) ->
	io:format("Starting layers_test_app (init) with ~p~n", [Config]).
	
test() ->
	converse:open_and_send({0,0,0,0}, ?PORT, {data, whisper:encrypt("hi")}).

layers_receive() ->
	receive
		{data, Socket, Data} ->
			io:format("Received function ~p~n",[Data]),
			case Data of
				{data, Message} ->
					io:format("Received data ~p~n", [Message]),
					layers_receive();
				{who_are_you} ->
					io:format("Received who are you from ~p~n", [Socket]),
					layers_receive();
				Anything ->
					io:format("Received anything: ~p~n", [Anything]),
					layers_receive()
			end;
		Anything ->
			io:format("Received Anything: ~p~n", [Anything]),
			layers_receive()
	end.