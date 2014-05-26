#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp disable -sname tidyup@localhost -gproc gproc_dist all

-module(tidyup).

main(_) ->
    Files = filelib:wildcard("track.*"),
    ok = move_files(Files).

move_files([]) ->
    ok;
move_files([H|T]) ->
    DirName = compose_log_dirname(H),
    ok = filelib:ensure_dir(DirName),
    NewPath = filename:join([DirName, H]),
    Cmd = lists:append(["mv ", H, " ", NewPath]),
    P = erlang:open_port({spawn, Cmd},
                          [stderr_to_stdout, in, exit_status,
                          binary, stream, {line, 255}]),
    receive
        {P, {exit_status, 0}} ->
            move_files(T);
        {P, {exit_status, S}} ->
            io:format("Error while moving log files, status = ~p~n", [S])
    end.

compose_log_dirname(FileName) ->
    [_ | Ext] = filename:extension(FileName),
    lists:append([epoch_to_ymd(atoi(Ext)), "/"]).
    
epoch_to_ymd(MilliSeconds) ->
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds = BaseDate + (MilliSeconds div 1000),
    {{Year, Month, Day}, {_, _, _}} =
        calendar:gregorian_seconds_to_datetime(Seconds),
    lists:flatten(io_lib:fwrite("~4..0B/~2..0B/~2..0B", [Year, Month, Day])).

atoi([$- | String]) ->
    -1 * atoi(String, 0);
atoi(String) ->
    atoi(String, 0).

atoi([], Acc) ->
    Acc;
atoi([C | Rest], Acc) when C >= $0, C =< $9 ->
    atoi(Rest, 10 * Acc + (C - $0)).



    
