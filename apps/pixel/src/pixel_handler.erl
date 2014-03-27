-module(pixel_handler).
-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  {Query, Req2}       = cowboy_req:qs(Req),
  {Headers, Req3}     = cowboy_req:headers(Req2),
  {{IP, Port}, Req4}  = cowboy_req:peer(Req3),
  {Host, Req5}        = cowboy_req:host(Req4),
  {HostPort, Req6}    = cowboy_req:port(Req5),
  {Path, Req7}        = cowboy_req:path(Req6),
  {Pid, Req8}         = cowboy_req:qs_val(<<"pid">>, Req7, <<"">>),
  {Mid, Req9}         = cowboy_req:qs_val(<<"mid">>, Req8, <<"">>),
  {URL, Req10}        = cowboy_req:url(Req9),

  Doc = {[
    {ts, jstime(os:timestamp())},
    {client, {[
      {ip, addr_to_string(IP)},
      {port, Port}
    ]}},
    {resource, {[
      {url, URL},
      {host, Host},
      {port, HostPort},
      {path, Path},
      {query, Query}
    ]}},
    {pid, Pid},
    {mid, Mid},
    {headers, {Headers}}
  ]},

  JSONString = jiffy:encode(Doc),
  CR = <<13>>,
  writer_srv:log(<<JSONString/binary, CR/binary>>),

  {ok, Req11} = cowboy_req:reply(200, [
      {<<"content-type">>, <<"text/plain">>},
      {<<"connection">>, <<"close">>}
  ], <<"Hello, World XXXXXX!">>, Req10),
  {ok, Req11, State}.

terminate(_Reason, _Req, _State) ->
    ok.

addr_to_string(Addr) ->
  list_to_binary(string:join([integer_to_list(S) || S <- tuple_to_list(Addr)], ".")).

jstime({Mega, Sec, Micro}) ->
  round(((Mega * 1000000) + Sec + (Micro / 1000000)) * 1000).

