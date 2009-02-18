-module (layers).
-author ("Ari Lerner").
-include ("layers.hrl").

-export ([start/2, running_receiver/1, pass/2]).
-export ([init/0, start/0, add/2]).
-export ([start_bundle/1]).
-export ([registered_name/1,register_process/2]).

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
			start_bundle([
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

start_bundle(Arr) ->
	lists:foreach(fun({Name, Process}) -> 
		io:format("Starting ~s...~n", [Name]),
		Process(),
		io:format("Done.~n") end,
	Arr).

start_layers(Layers) ->
	ConstructedArray = construct_tuples(lists:reverse(Layers)),
	[ start_application(App, Successor, Config) || [App, Config, Successor] <- ConstructedArray ].
	
start_layers(Layers, Config) ->
	ConstructedArray = construct(Layers),
	[ start_application(App, Successor, Config) || [App, Successor] <- ConstructedArray ].

start_application(App, Successor, Config) ->
	StartConfig = config:update(successor, [Successor], Config),
	Pid = App:start(normal, StartConfig),
	receive
		Anything -> io:format("Caught exception ~p~n", [Anything])
		after 1000 -> ok 
	end,
	Pid.
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
	% [H] = Array,
	% lists:append(Acc, [[H, H]]);
	Acc;
construct0(Array, Acc) when length(Array) > 0 ->
	[H|T] = Array, [S|Rest] = T,
	NewAcc = lists:append(Acc, [[H,S]]), NewArray = [S|Rest],
	construct0(NewArray, NewAcc).
	
% Convert and array of tuples to an array of 
% successors with their tuple
% such as
% [{bob, "man"}, {jane, "woman"}]
% ->
% [[bob, "man", jane], [jane, "woman", jane]]
construct_tuples(Array) -> construct_tuples0(Array, []).
construct_tuples0([], Acc) -> Acc;
construct_tuples0(Array, Acc) when length(Array) =:= 1 -> 
	% [{H,Config}] = Array,
	% lists:append(Acc, [[H,Config,H]]);
	Acc;
construct_tuples0(Arr, Acc) ->
	[{H,Config}|T] = Arr, [{SName, SConfig}|Rest] = T,
	NewAcc = lists:append(Acc, [[H, Config, SName]]), NewArray = [{SName, SConfig}|Rest],
	construct_tuples0(NewArray, NewAcc).
	
pass(SuccessorFun, Msg) ->
	case running_receiver(SuccessorFun) of
		{pid, Pid} -> Pid ! Msg;
		Anything -> io:format("Received ~p in pass(~p,~p)~n", [Anything, SuccessorFun, Msg])
	end.

running_receiver(Mfa) ->
	M = registered_name(Mfa),
	case erlang:whereis(M) of
		undefined -> P = run_fun(Mfa), {pid, P};
		Pid -> {pid, Pid}
	end.

register_process(Mfa, Pid) -> 
	% erlang:register(registered_name(Mfa), Pid).
	ok.
	
registered_name([M,F]) -> erlang:list_to_atom(lists:flatten(io_lib:format("layers~p~p", [M,F])));
registered_name([M]) -> erlang:list_to_atom(lists:flatten(io_lib:format("layers~p", [M])));
registered_name(M) -> erlang:list_to_atom(lists:flatten(io_lib:format("layers~p", [M]))).

run_fun([M,F,A]) ->
	erlang:spawn_link(M,F,A);
	
run_fun([M,F]) ->
	A = [],
	run_fun([M,F,A]);
	
run_fun([M]) ->
	F = layers_receive,
	A = [],
	run_fun([M,F,A]);

run_fun(undefined) ->
	io:format("Error: undefined successor~n").
	
start_child(Mod) ->
    {ok,_} = supervisor:start_child(layers_sup,
                                    {Mod, {Mod, start_link, []},
                                     transient, 100, worker, [Mod]}),
    ok.
