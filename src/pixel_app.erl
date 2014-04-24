-module(pixel_app).
-author("wal").

-behavior(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    init_metrics(),
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

init_metrics() ->
    folsom_metrics:new_histogram({pixel, cowboy, p_img_request}, slide_uniform, {60, 1028}),
    folsom_metrics:new_counter({pixel, log_mover, copy_initiated}),
    folsom_metrics:new_counter({pixel, log_mover, total_bytes}),
    folsom_metrics:new_counter({pixel, log_mover, copy_succeeded}), 
    folsom_metrics:new_counter({pixel, log_mover, copy_failed}),
    ok.










