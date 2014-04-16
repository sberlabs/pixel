-module(pixel_app).
-author("wal").

-behavior(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, HttpPort} = application:get_env(http_port),
    Dispatch = cowboy_router:compile([
                                      %% {URIHost, list({URIPath, Handler, Opts})}
                                      {'_', [{"/img/p.png", pixel_handler, []}]}
                                     ]),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(my_http_listener, 100,
                      [{port, HttpPort}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),
    pixel_sup:start_link().

stop(_State) ->
    ok.
