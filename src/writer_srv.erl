-module(writer_srv).
-author("wal").

-behaviour(gen_server).

-export([start_link/0, log/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

log(Data) ->
  gen_server:cast({global, ?SERVER}, {log, Data}).

init([]) ->
  {ok, Log} = disk_log:open([{name, track},
                             {type, wrap},
                             {format, external},
                             {size, {1048576, 10}}]),
  {ok, Log}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({log, Data}, State) ->
  disk_log:balog(State, Data),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
