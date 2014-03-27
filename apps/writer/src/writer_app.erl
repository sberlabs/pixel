-module(writer_app).
-author("wal").

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  writer_sup:start_link().

stop(_State) ->
  ok.
