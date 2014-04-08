-module(pixel_sup).
-author("wal").

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Writer = {writer_srv, {writer_srv, start_link, []},
              permanent, 2000, worker, [writer_srv]},
    Children = [Writer],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.


