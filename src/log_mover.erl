-module(log_mover).
-author("wal").

-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, ArchiveDir} = application:get_env(log_archive),
    gproc_ps:subscribe(g, log_wrap),
    {ok, filename:absname(ArchiveDir)}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, log_wrap, Info}, ArchiveDir) ->
    TS = util:jstime(os:timestamp()),
    LogName = erlang:atom_to_list(proplists:get_value(name, Info)),
    {_MaxLen, MaxNum} = proplists:get_value(size, Info),
    LogFile = proplists:get_value(file, Info),
    LogFileNum = proplists:get_value(current_file, Info),
    PrevFileNum = case LogFileNum - 1 of 0 -> MaxNum; _ -> LogFileNum - 1 end,
    LogFilePath = lists:append([LogFile, ".", integer_to_list(PrevFileNum)]),
    ArchiveFileName = lists:append([LogName, ".", integer_to_list(TS)]),
    YMD_DirName = filename:join([ArchiveDir, compose_ymd_dirname(TS)]),
    ArchiveFilePath = filename:join([YMD_DirName, ArchiveFileName]), 
    ok = filelib:ensure_dir(lists:append([YMD_DirName, "/"])),
    Cmd = lists:append(["cp ", LogFilePath, " ", ArchiveFilePath]),
    _P = erlang:open_port({spawn, Cmd},
                          [stderr_to_stdout, in, exit_status,
                          binary, stream, {line, 255}]),
    folsom_metrics:notify({{pixel, log_mover, copy_initiated}, {inc, 1}}),
    {noreply, ArchiveDir};
handle_info({_P, {exit_status, 0}}, State) ->
    {ok, {Size, _Num}} = application:get_env(log_size),
    folsom_metrics:notify({{pixel, log_mover, copy_succeeded}, {inc, 1}}),
    folsom_metrics:notify({{pixel, log_mover, total_bytes}, {inc, Size}}),
    {noreply, State};
handle_info({_P, {exit_status, _S}}, State) ->
    folsom_metrics:notify({{pixel, log_mover, copy_failed}, {inc, 1}}),
    {noreply, State};
handle_info({_P, {data, {eol, S}}}, State) ->
    io:format("~p~n", [S]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

compose_ymd_dirname(MilliSeconds) ->
    lists:append([epoch_to_ymd(MilliSeconds), "/"]).
    
epoch_to_ymd(MilliSeconds) ->
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds = BaseDate + (MilliSeconds div 1000),
    {{Year, Month, Day}, {_, _, _}} =
        calendar:gregorian_seconds_to_datetime(Seconds),
    lists:flatten(io_lib:fwrite("~4..0B/~2..0B/~2..0B", [Year, Month, Day])).

