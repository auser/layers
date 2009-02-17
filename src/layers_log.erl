-module (layers_log).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([debug/1, debug/2, message/4, info/1, info/2,
         warning/1, warning/2, error/1, error/2]).

-import(io).
-import(error_logger).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

debug(Fmt) ->
    gen_server:cast(?SERVER, {debug, Fmt}).

debug(Fmt, Args) when is_list(Args) ->
    gen_server:cast(?SERVER, {debug, Fmt, Args}).

message(Direction, Channel, MethodRecord, Content) ->
    gen_server:cast(?SERVER,
		    {message, Direction, Channel, MethodRecord, Content}).

info(Fmt) ->
    gen_server:cast(?SERVER, {info, Fmt}).

info(Fmt, Args) when is_list(Args) ->
    gen_server:cast(?SERVER, {info, Fmt, Args}).

warning(Fmt) ->
    gen_server:cast(?SERVER, {warning, Fmt}).

warning(Fmt, Args) when is_list(Args) ->
    gen_server:cast(?SERVER, {warning, Fmt, Args}).

error(Fmt) ->
    gen_server:cast(?SERVER, {error, Fmt}).

error(Fmt, Args) when is_list(Args) ->
    gen_server:cast(?SERVER, {error, Fmt, Args}).

%%--------------------------------------------------------------------

init([]) -> {ok, none}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({debug, Fmt}, State) ->
    io:format("debug:: "), io:format(Fmt),
    error_logger:info_msg("debug:: " ++ Fmt),
    {noreply, State};
handle_cast({debug, Fmt, Args}, State) ->
    io:format("debug:: "), io:format(Fmt, Args),
    error_logger:info_msg("debug:: " ++ Fmt, Args),
    {noreply, State};
handle_cast({message, Direction, Channel, MethodRecord, Content}, State) ->
    io:format("~s ch~p ~p~n",
	      [case Direction of
		   in -> "-->";
		   out -> "<--" end,
	       Channel,
	       {MethodRecord, Content}]),
    {noreply, State};
handle_cast({info, Fmt}, State) ->
    error_logger:info_msg(Fmt),
    {noreply, State};
handle_cast({info, Fmt, Args}, State) ->
    error_logger:info_msg(Fmt, Args),
    {noreply, State};
handle_cast({warning, Fmt}, State) ->
    error_logger:warning_msg(Fmt),
    {noreply, State};
handle_cast({warning, Fmt, Args}, State) ->
    error_logger:warning_msg(Fmt, Args),
    {noreply, State};
handle_cast({error, Fmt}, State) ->
    error_logger:error_msg(Fmt),
    {noreply, State};
handle_cast({error, Fmt, Args}, State) ->
    error_logger:error_msg(Fmt, Args),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

