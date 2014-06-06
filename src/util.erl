-module(util).
-author(wal).

-export([addr_to_string/1, jstime/1, jstime_micro/1, atoi/1]).

addr_to_string(Addr) ->
    list_to_binary(string:join([integer_to_list(S) || S <- tuple_to_list(Addr)], ".")).

jstime({Mega, Sec, Micro}) ->
    round(((Mega * 1000000) + Sec + (Micro / 1000000)) * 1000).

jstime_micro({Mega, Sec, Micro}) ->
    Mega*1000000000000 + Sec*1000000 + Micro.

atoi([$- | String]) ->
    -1 * atoi(String, 0);
atoi(String) ->
    atoi(String, 0).

atoi([], Acc) ->
    Acc;
atoi([C | Rest], Acc) when C >= $0, C =< $9 ->
    atoi(Rest, 10 * Acc + (C - $0)).
