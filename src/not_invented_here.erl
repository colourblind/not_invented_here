-module(not_invented_here).
-behaviour(application).

-include("config.hrl").

-export([start/0, start/1]).
-export([start/2, stop/1, no_really_start/0]).

start(normal, _) ->
    case application:get_application() of
        {ok, ApplicationName} ->
            io:format("Starting application: ~p~n", [ApplicationName]),
            start();
        Error ->
            Error
    end.
 
stop(_) ->
    ok.
    
no_really_start() ->
    % Since we can't use -run from the command line as it passes arguments
    % in a list . . .
    application:start(?MODULE).

start() ->
    start(5678).
    
start(Port) ->
    io:format("Starting up~n"),
    {ok, ServerPid} = state:start_link(),
    io:format("Created server process ~p~n", [ServerPid]),
    {ok, LSocket} = gen_tcp:listen(Port, [{active, true}, {packet, line}]),
    listener(LSocket, ServerPid).
    
listener(LSocket, ServerPid) ->
    io:format("Listening~n"),
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            Pid = spawn(handshake, start_link, [ServerPid, Socket]),
            case gen_tcp:controlling_process(Socket, Pid) of
                ok ->
                    io:format("Control for socket ~p transferred to ~p~n", [Socket, Pid]);
                {error, Reason} ->
                    io:format("Control for socket ~p transfer failed! ~p~n", [Socket, Reason])
            end,
            listener(LSocket, ServerPid);
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            gen_tcp:close(LSocket)
    end.
