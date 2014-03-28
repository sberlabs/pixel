-module(pixel_handler).
-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(TENYEARS, 10*365*24*60*60).
-define(CR, 13).
-define(PIXEL_PNG, "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABAQMAAAAl21bKAAAAA1BMVEUAAACnej3aAAAAAXRSTlMAQObYZgAAAApJREFUCNdjYAAAAAIAAeIhvDMAAAAASUVORK5CYII=").

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

  UUID = list_to_binary(uuid:to_string(uuid:uuid4())),
  {Cookie, Req11} = cowboy_req:cookie(<<"sberlabspx">>, Req10, UUID),
  Req12 = cowboy_req:set_resp_cookie(<<"sberlabspx">>, Cookie, [{max_age, ?TENYEARS}], Req11),

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
    {sberlabspx, Cookie},
    {pid, Pid},
    {mid, Mid},
    {headers, {Headers}}
  ]},

  JSONString = jiffy:encode(Doc),
  CR = <<13>>,
  writer_srv:log(<<JSONString/binary, CR/binary>>),

  {ok, Req13} = cowboy_req:reply(200, [
      {<<"content-type">>, <<"image/png">>},
      {<<"connection">>, <<"close">>}
  ], base64:decode(?PIXEL_PNG), Req12),
  {ok, Req13, State}.

terminate(_Reason, _Req, _State) ->
    ok.

addr_to_string(Addr) ->
  list_to_binary(string:join([integer_to_list(S) || S <- tuple_to_list(Addr)], ".")).

jstime({Mega, Sec, Micro}) ->
  round(((Mega * 1000000) + Sec + (Micro / 1000000)) * 1000).

