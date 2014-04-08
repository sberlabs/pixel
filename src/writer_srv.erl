-module(writer_srv).
-author("wal").

-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(LF, <<10>>).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->

    %% Open log file, create it if needed
    {ok, Log} = disk_log:open([{name, track},
                              {type, wrap},
                              {format, external},
                              {size, {1048576, 10}}]),

    %% Register process in pixel_data process group
    gproc_ps:subscribe(g, pixel_data),

    {ok, Log}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, pixel_data, Data}, State) ->
    %% Convert request data into JSON string and log it asynchronously
    JSONString = jiffy:encode(Data),
    disk_log:balog(State, <<JSONString/binary, ?LF/binary>>),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
