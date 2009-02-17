-module (layers).
-author ("Ari Lerner").
-include ("layers.hrl").

-export ([start/2, running_receiver/1, pass/2]).

% Text exports
-export ([construct/1]).

start(Layers, Config) ->
	F = fun([App, Successor]) -> 
		% process_flag(trap_exit, true),
		App:start(normal, config:update(successor, [Successor], Config)),
		receive
			Anything ->
				io:format("Caught exception ~p~n", [Anything])
			after 1000 -> ok
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
	
% running_receiver(undefined, Fun) ->
% 		run_fun(Fun);
% 
% running_receiver(Pid, Fun) when is_pid(Pid) ->
% 	case is_process_alive(Pid) of
% 		true -> Pid;
% 		false ->run_fun(Fun)
% 	end.

pass(SuccessorFun, Msg) ->
	Pid = running_receiver(SuccessorFun),
	Pid ! Msg.

running_receiver([M,F,A]) ->
	case erlang:whereis(M) of
		undefined -> run_fun([M,F,A]);
		Pid -> Pid
	end.

run_fun([M,F,A]) ->
	Pid = proc_lib:spawn_link(M,F,A),
	erlang:register(M, Pid),
	Pid;
	
run_fun([M,F]) ->
	A = [self()],
	run_fun([M,F,A]);
	
run_fun([M]) ->
	F = layers_receive,
	A = [self()],
	run_fun([M,F,A]).