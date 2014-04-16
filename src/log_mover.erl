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
    LogName = erlang:atom_to_list(proplists:get_value(name, Info)),
    {_MaxLen, MaxNum} = proplists:get_value(size, Info),
    LogFile = proplists:get_value(file, Info),
    LogFileNum = proplists:get_value(current_file, Info),
    PrevFileNum = case LogFileNum - 1 of 0 -> MaxNum; _ -> LogFileNum - 1 end,
    LogFilePath = lists:append([LogFile, ".", integer_to_list(PrevFileNum)]),
    TS = integer_to_list(util:jstime(os:timestamp())),
    ArchiveFileName = lists:append([LogName, ".", TS]),
    ArchiveFilePath = filename:join([ArchiveDir, ArchiveFileName]), 
    Cmd = lists:append(["cp ", LogFilePath, " ", ArchiveFilePath]),
    _P = erlang:open_port({spawn, Cmd},
                          [stderr_to_stdout, in, exit_status,
                          binary, stream, {line, 255}]),
    {noreply, ArchiveDir};
handle_info({_P, {exit_status, 0}}, State) ->
    %% file copied successfully
    {noreply, State};
handle_info({_P, {exit_status, _S}}, State) ->
    %% file copy error
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
