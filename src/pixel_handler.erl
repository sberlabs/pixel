-module(pixel_handler).
-author("wal").

-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(TENYEARS, 10*365*24*60*60).
-define(PIXEL_PNG, "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABAQMAAAAl21bKAAAAA1BMVEUAAACnej3aAAAAAXRSTlMAQObYZgAAAApJREFUCNdjYAAAAAIAAeIhvDMAAAAASUVORK5CYII=").

init(_Type, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    Start = util:jstime_micro(erlang:now()),
    %% Get headers and other properties from incoming request
    {Query, Req2}       = cowboy_req:qs(Req),
    {Headers, Req3}     = cowboy_req:headers(Req2),
    {{IP, Port}, Req4}  = cowboy_req:peer(Req3),
    {Host, Req5}        = cowboy_req:host(Req4),
    {HostPort, Req6}    = cowboy_req:port(Req5),
    {Path, Req7}        = cowboy_req:path(Req6),
    {Pid, Req8}         = cowboy_req:qs_val(<<"pid">>, Req7, <<"">>),
    {Mid, Req9}         = cowboy_req:qs_val(<<"mid">>, Req8, <<"">>),
    {URL, Req10}        = cowboy_req:url(Req9),
    %% Read the cookie (set it to generated UUID if absent) and set the cookie in our response
    UUID = list_to_binary(uuid:to_string(uuid:uuid4())),
    {Cookie, Req11} = get_cookie_safe(<<"sberlabspx">>, Req10, UUID),
    Req12 = cowboy_req:set_resp_cookie(<<"sberlabspx">>, Cookie, [{max_age, ?TENYEARS}], Req11),
    %% Prepare request data to be published to subscribers
    Data = {[
             {ts, util:jstime(os:timestamp())},
             {client, {[
                        {ip, util:addr_to_string(IP)},
                        {port, Port}
                       ]}},
             {resource, {[
                          {url, URL},
                          {host, Host},
                          {port, HostPort},
                          {path, Path},
                          {q, Query}
                         ]}},
             {cookie, Cookie},
             {pid, Pid},
             {mid, Mid},
             {headers, {Headers}}
            ]},
    %% Publish data to pixel_data process group
    gproc_ps:publish(g, pixel_data, Data),
    %% Reply to original request, serve invisible tracking pixel
    {ok, Req13} = cowboy_req:reply(200, [
                                         {<<"content-type">>, <<"image/png">>},
                                         {<<"connection">>, <<"close">>}
                                        ], base64:decode(?PIXEL_PNG), Req12),
    Finish = util:jstime_micro(erlang:now()),
    folsom_metrics:notify({{pixel, cowboy, p_img_request}, Finish - Start}),
    {ok, Req13, State}.

terminate(_Reason, _Req, _State) ->
    ok.

get_cookie_safe(Name, Req, DefVal) ->
    case catch cowboy_req:cookie(Name, Req, DefVal) of
        {'EXIT', _} ->
            %% Handle malformed 'cookie' header on some mobile devices/browsers
            {DefVal, Req};
        {Cookie, Req1} ->
            {Cookie, Req1}
    end.
    








