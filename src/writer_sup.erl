-module(writer_sup).
-author("wal").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({global, ?SERVER}, ?MODULE, []).

init([]) ->
  Server = {writer_srv, {writer_srv, start_link, []},
    permanent, 2000, worker, [writer_srv]},
  Children = [Server],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
