-module (layers_app).

-behaviour(application).

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).

%% Application and Supervisor callbacks
-export([start/2]).
	
start(_Type, StartArgs) ->
	[Module, Args] = StartArgs,
	supervisor:start_link({local, Module}, ?MODULE, [Module, Args]).

% stop(_S) ->
%     ok.
% 
% init([Module, Args]) ->
% 	{ok,
% 		{_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
% 			[
% 				{ Module,{Module,start_link,[Args]}, permanent,2000,worker,[Module] }
% 			]
% 		}
% 	}.