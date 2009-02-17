-module (layers).
-author ("Ari Lerner").
-include ("layers.hrl").

-export ([start/2, running_receiver/1, pass/2]).
-export ([init/0, start/0, add/2]).

% Text exports
-export ([construct/1, construct_tuples/1]).
-record (layer, {name, config}).
-record (state, {layers = []}).

% Init the layers
init() -> 
	Pid = spawn_link(fun() -> init_receive([]) end),
	erlang:register(layers_processes, Pid).
	
init_receive(LayersWithConfig) ->
	receive
		{add, Name, Config} ->
			NewLayersWithConfig = [{Name, Config}|LayersWithConfig],
			init_receive(NewLayersWithConfig);
		{done} ->
			lists:foreach(fun({Name, Process}) -> 
				io:format("Starting ~p~n", [Name]),
				Process(),
				io:format("Started.~n") end,
				[
					{"Layers supervisor", fun() -> layers_sup:start_link() end},
					{"Logger", fun() -> start_child(layers_log) end},			
					{"Layers", fun() -> start_layers(LayersWithConfig) end}
				])
	after 10000 ->
		io:format("Error timeout~n"),
		exit({error, timeout})
	end.
	
start() -> 
	Pid = whereis(layers_processes),
	Pid ! {done}.
add(Name, Config) -> 
	Pid = whereis(layers_processes),
	Pid ! {add, Name, Config}.
	
start(Layers, Config) ->
	lists:foreach(fun({Name, Process}) -> 
		io:format("Starting ~p~n", [Name]),
		Process(),
		io:format("Started.~n") end,
		[
			{"Layers supervisor", fun() -> layers_sup:start_link() end},
			{"Logger", fun() -> start_child(layers_log) end},			
			{"Layers", fun() -> start_layers(Layers, Config) end}
		]).

start_layers(Layers) ->
	ConstructedArray = construct(Layers),
	[ start_application(App, Successor, Config) || [{App, Config}, Successor] <- ConstructedArray ].
	
start_layers(Layers, Config) ->
	ConstructedArray = construct(Layers),
	[ start_application(App, Successor, Config) || [App, Successor] <- ConstructedArray ].

start_application(App, Successor, Config) ->
	App:start(normal, config:update(successor, [Successor], Config)),
	receive
		Anything -> io:format("Caught exception ~p~n", [Anything])
		after 1000 -> ok 
	end.
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
construct0(Array, Acc) when length(Array) =:= 1 -> 
	[H] = Array,
	lists:append(Acc, [[H, undefined]]);
construct0(Array, Acc) when length(Array) > 0 ->
	[H|T] = Array, [S|Rest] = T,
	NewAcc = lists:append(Acc, [[H,S]]), NewArray = [S|Rest],
	construct0(NewArray, NewAcc).
	
% Convert and array of tuples to an array of 
% successors with their tuple
% such as
% [{bob, "man"}, {jane, "woman"}]
% ->
% [[bob, "man", jane], [jane, "woman", undefined]]
construct_tuples(Array) -> construct_tuples0(Array, []).
construct_tuples0([], Acc) -> Acc;
construct_tuples0(Array, Acc) when length(Array) =:= 1 -> 
	[{H,Config}] = Array,
	lists:append(Acc, [[H,Config,undefined]]);
construct_tuples0(Arr, Acc) ->
	[{H,Config}|T] = Arr, [{SName, SConfig}|Rest] = T,
	NewAcc = lists:append(Acc, [[H, Config, SName]]), NewArray = [{SName, SConfig}|Rest],
	construct_tuples0(NewArray, NewAcc).
	
pass(SuccessorFun, Msg) ->
	case running_receiver(SuccessorFun) of
		Pid -> Pid ! Msg;
		ok -> ok
	end.

running_receiver(undefined) -> ok;
running_receiver(Mfa) ->
	M = erlang:hd(Mfa),
	case erlang:whereis(M) of
		undefined -> run_fun(Mfa);
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
	run_fun([M,F,A]);

run_fun(undefined) ->
	io:format("Error: undefined successor~n").
	
start_child(Mod) ->
    {ok,_} = supervisor:start_child(layers_sup,
                                    {Mod, {Mod, start_link, []},
                                     transient, 100, worker, [Mod]}),
    ok.
