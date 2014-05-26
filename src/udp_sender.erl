-module(udp_sender).
-author("wal").

-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(LF, <<10>>).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
    %% Open UDP socket
    {ok, Socket} = gen_udp:open(0, [binary]),
    %% Register process in pixel_data process group
    gproc_ps:subscribe(g, pixel_data),
    {ok, Socket}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, pixel_data, Data}, Socket) ->
    %% Convert request data into JSON string and log it asynchronously
    try jiffy:encode(Data) of
        JSONString ->
            gen_udp:send(Socket, "localhost", 6363, <<JSONString/binary, ?LF/binary>>),
            {noreply, Socket}
    catch
        _:Err ->
            io:format("EXCEPTION CAUGHT: jiffy error '~p', here is the data it caused:~n~p~n", [Err, Data]),
            {noreply, Socket}
    end.

terminate(_Reason, Socket) ->
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
