-module(util).
-author(wal).

-export([addr_to_string/1, jstime/1]).

addr_to_string(Addr) ->
    list_to_binary(string:join([integer_to_list(S) || S <- tuple_to_list(Addr)], ".")).

jstime({Mega, Sec, Micro}) ->
    round(((Mega * 1000000) + Sec + (Micro / 1000000)) * 1000).
