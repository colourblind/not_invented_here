-module(not_invented_here).

-include("config.hrl").

-export([start/0, start/1]).

start() ->
    start(5678).
    
start(Port) ->
    io:format("Starting up~n"),
    {ok, LSocket} = gen_tcp:listen(Port, [{active, false}, {packet, line}]),
    listener(LSocket).
    
listener(LSocket) ->
    io:format("Listening~n"),
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            io:format("Got connection~p~n", [Socket]),
            welcome(Socket),
            {ok, Pid} = client_handler:start_link(self(), Socket),
            io:format("Created client ~p~n", [Pid]),
            inet:setopts(Socket, [{active, true}]),
            % Chance of losing messages before we switch controlling process? :-/
            case gen_tcp:controlling_process(Socket, Pid) of
                ok ->
                    io:format("Control for socket ~p transferred~n", [Socket]);
                {error, Reason} ->
                    io:format("Control for socket ~p transfer failed! ~p~n", [Socket, Reason])
            end,
            listener(LSocket);
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            gen_tcp:close(LSocket)
    end.
    
welcome(Socket) ->
    send(Socket, "NOTICE AUTH :*** Checking Ident\r\n"),
    send(Socket, "NOTICE AUTH :*** No ident response\r\n"),
    {ok, NickLine} = gen_tcp:recv(Socket, 0),
    io:format("NICKLINE = ~p~n", [NickLine]),
    Nick = lists:nth(2, string:tokens(NickLine, " \n")),
    io:format("NICK = ~p~n", [Nick]),
    {ok, UserLine} = gen_tcp:recv(Socket, 0),
    UserTokens = string:tokens(UserLine, " \n"),
    Username = lists:nth(2, UserTokens),
    ClientHost = lists:nth(3, UserTokens),
    ServerName = lists:nth(4, UserTokens),
    RealName = lists:nth(5, UserTokens),
    io:format("USER = ~p ~p ~p ~p~n", [Username, ClientHost, ServerName, RealName]),
    % check NICK
    % store NICK if good
    % collect USER details
    send(Socket, "PING :123456789\r\n"),
    wait(Socket),
    % check PING response
    % send server details
    send(Socket, ":" ++ ?SERVER_NAME ++ " 001 " ++ Nick ++ " :Welcome to " ++ ?SERVER_NAME ++ "\r\n"),
    send(Socket, ":" ++ ?SERVER_NAME ++ " 002 " ++ Nick ++ " :Powered by not_invented_here\r\n"),
    % DONE
    send(Socket, "PING :" ++ ?SERVER_NAME ++ "\r\n"),
    wait(Socket),
    % move to handler (handle USERHOST
    send(Socket, ":" ++ ?SERVER_NAME ++ " 302 " ++ Nick ++ " :" ++ Nick ++ "=+~" ++ Username ++ "@" ++ ?SERVER_NAME ++ "\r\n").

    
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
