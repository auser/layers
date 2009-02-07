-module (layers).
-author ("Ari Lerner").

-export ([init/1, running_receiver/2]).

% Run the layers with the layer supervisor
init(Layers) ->
	F = fun(AppArry) -> 
		io:format("Constructing ~p~n", [AppArry]),
		% process_flag(trap_exit, true),
		layers_app:start(normal, AppArry),
		receive
			Anything ->
				io:format("Caught exception ~p~n", [Anything])
		end
	end,
	ConstructedArray = construct(Layers),
	[ F(Layer) || Layer <- ConstructedArray ].
	
% Construct an array that has both the layer and the successor
% such as
% [Layer, Successor]
% from an array
% [1,2,3,4]
% ->
% [[1,2],[2,3],[3,4]]
construct(Array) ->
	construct0(Array, []).

construct0([], Acc) -> Acc;
construct0(Array, Acc) when length(Array) =:= 1 -> Acc;
construct0(Array, Acc) when length(Array) > 0 ->
	[H|T] = Array, 
	[S|Rest] = T,
	NewAcc = lists:append(Acc, [[H,S]]),
	NewArray = [S|Rest],
	construct0(NewArray, NewAcc).
	
running_receiver(undefined, Fun) ->
		run_fun(Fun);

running_receiver(Pid, Fun) when is_pid(Pid) ->
	case is_process_alive(Pid) of
		true -> Pid;
		false ->run_fun(Fun)
	end.

run_fun(Fun) ->
	case length(Fun) of
		2 -> [M,F] = Fun;
		1 -> [M] = Fun, F = receive_function
	end,
	A = [self()],
	proc_lib:spawn_link(M,F,A).