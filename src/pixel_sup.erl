-module(pixel_sup).
-author("wal").

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Writer = {log_writer, {log_writer, start_link, []},
              permanent, 2000, worker, [log_writer]},
    Mover  = {log_mover, {log_mover, start_link, []},
              permanent, 2000, worker, [log_mover]},
    Children = [Writer, Mover],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.


