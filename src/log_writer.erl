-module(log_writer).
-author("wal").

-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(LF, <<10>>).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, Name} = application:get_env(log_name),
    {ok, File} = application:get_env(log_file),
    {ok, LogDir} = application:get_env(log_dir),
    {ok, {Size, Num}} = application:get_env(log_size),
    %% Open log file, create it if needed
    {ok, Log} = disk_log:open([{name, Name},
                               {file, filename:absname(filename:join([LogDir, File]))},
                               {type, wrap},
                               {format, external},
                               {size, {Size, Num}},
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
    try jiffy:encode(Data) of
        JSONString ->
            disk_log:balog(State, <<JSONString/binary, ?LF/binary>>),
            {noreply, State}
    catch
        _:Err ->
            io:format("EXCEPTION CAUGHT: jiffy error '~p', here is the data it caused:~n~p~n", [Err, Data]),
            {noreply, State}
    end;
    %%JSONString = jsx:encode(Data),
    %%disk_log:balog(State, <<JSONString/binary, ?LF/binary>>),
    %%{noreply, State};
handle_info({disk_log, _Node, Log, {wrap, _}}, State) ->
    %% Notify subscribers that log file is wrapped
    Info = disk_log:info(Log),
    gproc_ps:publish(g, log_wrap, Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
