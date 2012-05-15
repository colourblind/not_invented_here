-module(not_invented_here).

-include("config.hrl").

-export([start/0, start/1]).

start() ->
    start(5678).
    
start(Port) ->
    io:format("Starting up~n"),
    {ok, LSocket} = gen_tcp:listen(Port, [{active, false}, {packet, line}, binary]),
    listener(LSocket).
    
listener(LSocket) ->
    io:format("Listening~n"),
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            io:format("Got connection~p~n", [Socket]),
            welcome(Socket),
            spawn(fun() -> loop(Socket) end),
            listener(LSocket);
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            gen_tcp:close(LSocket)
    end.
    
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            io:format("-> ~p~n", [Packet]),
            loop(Socket);
        {error, Reason} ->
            io:format("Recv error: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.
    
    
welcome(Socket) ->
    send(Socket, "NOTICE AUTH :*** Checking Ident\r\n"),
    send(Socket, "NOTICE AUTH :*** No ident response\r\n"),
    wait(Socket),
    % check NICK
    % store NICK if good
    % collect USER details
    send(Socket, "PING :123456789\r\n"),
    wait(Socket),
    % check PING response
    % send server details
    send(Socket, ":" ++ ?SERVER_NAME ++ " 001 Pike65 :Welcome to " ++ ?SERVER_NAME ++ "\r\n"),
    send(Socket, ":" ++ ?SERVER_NAME ++ " 002 Pike65 :Powered by not_invented_here\r\n"),
    % DONE
    send(Socket, "PING :" ++ ?SERVER_NAME ++ "\r\n"),
    wait(Socket),
    % move to handler (handle USERHOST
    send(Socket, ":" ++ ?SERVER_NAME ++ " 302 Pike65 :Pike65=+~a@" ++ ?SERVER_NAME ++ "\r\n").

    
wait(Socket) ->
    case gen_tcp:recv(Socket, 0, 200) of
        {ok, Packet} ->
            io:format("-> ~p~n", [Packet]),
            wait(Socket);
        {error, Reason} ->
            io:format("Recv empty! Bailing! (~p)~n", [Reason])
    end.    
    
send(Socket, Data) ->
    io:format("<- ~p~n", [Data]),
    gen_tcp:send(Socket, Data).
    
% -> NICK nick
% -> USER username host server.name realname
% <- PING :123456789
% -> PONG 123456789
% <- :server.name 001 :Here goes a load of text
% <- :server.name 002 :and some more
