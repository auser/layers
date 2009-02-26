-module (layers).
-author ("Ari Lerner").
-include ("layers.hrl").

-export ([start/2, pass/2]).
-export ([init/0, start/0, add/2]).
-export ([start_bundle/1, start_child/2]).

% Text exports
-export ([construct/1, construct_tuples/1]).

% Init the layers
init() -> 
	Pid = spawn(fun() -> init_receive([]) end),
	global:register_name(layers_processes, Pid).
	
init_receive(LayersWithConfig) ->
	receive
		{add, Name, Config} ->
			NewLayersWithConfig = [{Name, Config}|LayersWithConfig],
			init_receive(NewLayersWithConfig);
		{done} ->
			start_custom_layers(LayersWithConfig),
			timer:sleep(2000),
			io:format("After started in block~n"),
			init_receive(LayersWithConfig);
		Anything ->
			io:format("Received anything: ~p~n", [Anything]),
			init_receive(LayersWithConfig)
	end.
	
start() -> 
	case global:whereis_name(layers_processes) of
	  Pid -> Pid ! {done};
	  {error, Reason} -> ok
	end.
	
add(Name, Config) -> 
	Pid = global:whereis_name(layers_processes),
	Pid ! {add, Name, Config}.
	
start(Layers, Config) ->
	start_bundle([
			{"Layers supervisor", fun() -> {ok, P} = layers_sup:start_link(), unlink(P) end},
			{"Logger", fun() -> start_child(layers_log, []) end},			
			{"Layers", fun() -> start_layers(Layers, Config) end}
		]).

start_custom_layers(LayersWithConfig) ->
	start_bundle([
		{"Layers supervisor", fun() -> 
				case layers_sup:start_link() of
					{ok,P} -> unlink(P);
					Anything -> ok
				end
			end},
		{"Logger", fun() -> start_child(layers_log, []) end},			
		{"Layers", fun() -> start_layers(LayersWithConfig) end}
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
	io:format("Start ~p with ~p~n", [App, StartConfig]),
	App:start(normal, StartConfig).
	% start_child(App, StartConfig).
	
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
	% Acc;
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
	[{H,Config}] = Array,
	lists:append(Acc, [[H,Config,undefined]]);
	% Acc;
construct_tuples0(Arr, Acc) ->
	[{H,Config}|T] = Arr, [{SName, SConfig}|Rest] = T,
	NewAcc = lists:append(Acc, [[H, Config, SName]]), NewArray = [{SName, SConfig}|Rest],
	construct_tuples0(NewArray, NewAcc).

pass([M,F,A], Msg) -> apply(M,F,A);	
pass([M,F], Msg) -> apply(M,F,[Msg]);
pass([M], Msg) -> apply(M,layers_receive,[Msg]).

start_child(Mod, []) ->
	supervisor:start_child(layers_sup,{Mod, {Mod, start_link, []}, permanent, 100, worker, [Mod]});
start_child(Mod, Args) ->
	supervisor:start_child(layers_sup,{Mod, {Mod, start_link, [Args]}, permanent, 100, worker, [Mod]}).
