-module(pixel_handler).
-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  % {AllValues, Req2} = cowboy_req:qs(Req),
  % {AllHeaders, Req3} = cowboy_req:headers(Req2),
  {{IP, Port}, Req4} = cowboy_req:peer(Req),
  Doc = jiffy:encode({[{ip, IP}, {port, Port}]}),
  io:format("~p~n", [Doc]),
  {ok, Req5} = cowboy_req:reply(200, [
      {<<"content-type">>, <<"text/plain">>}
  ], <<"Hello, World XXXXXX!">>, Req4),
  {ok, Req5, State}.

terminate(_Reason, _Req, _State) ->
    ok.

