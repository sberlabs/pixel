-module(writer_srv).
-author("wal").

-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(FILE_ATOM, track).
-define(FILE_NAME, "./log/track").
-define(FILE_SIZE, 5*1024*1024).
-define(FILE_NUM, 20).

-define(LF, <<10>>).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
    %% Open log file, create it if needed
    {ok, Log} = disk_log:open([{name, ?FILE_ATOM},
                               {file, ?FILE_NAME},
                               {type, wrap},
                               {format, external},
                               {size, {?FILE_SIZE, ?FILE_NUM}},
                               {notify, true}]),
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
    {noreply, State};
handle_info({disk_log, _Node, Log, {wrap, _}}, State) ->
    %% Notify subscribers that log file is wrapped
    gproc_ps:publish(g, log_wrap, Log),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
