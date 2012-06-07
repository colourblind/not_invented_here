-module(not_invented_here).

-include("config.hrl").
-include("records.hrl").

-export([start/0, start/1]).

start() ->
    start(5678).
    
start(Port) ->
    io:format("Starting up~n"),
    {ok, ServerPid} = irc:start_link(),
    io:format("Created server process ~p~n", [ServerPid]),
    {ok, LSocket} = gen_tcp:listen(Port, [{active, false}, {packet, line}]),
    listener(LSocket, ServerPid).
    
listener(LSocket, ServerPid) ->
    io:format("Listening~n"),
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            {Nick, Username, ClientHost, ServerName, RealName} = welcome(Socket),
            {ok, Pid} = client_handler:start_link(ServerPid, Socket),
            irc:add_user(ServerPid, #user{nick=Nick, clientPid=Pid, username=Username, clientHost=ClientHost, serverName=ServerName, realName=RealName}),
            io:format("Created client ~p~n", [Pid]),
            inet:setopts(Socket, [{active, true}]),
            % Chance of losing messages before we switch controlling process? :-/
            case gen_tcp:controlling_process(Socket, Pid) of
                ok ->
                    io:format("Control for socket ~p transferred~n", [Socket]);
                {error, Reason} ->
                    io:format("Control for socket ~p transfer failed! ~p~n", [Socket, Reason])
            end,
            listener(LSocket, ServerPid);
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            gen_tcp:close(LSocket)
    end.
    
welcome(Socket) ->
    % send(Socket, "NOTICE AUTH :*** Checking Ident\r\n"),
    % send(Socket, "NOTICE AUTH :*** No ident response\r\n"),
    {ok, NickLine} = wait(Socket, "NICK "),
    Nick = lists:nth(2, string:tokens(NickLine, " \n")),
    % TODO: check Nick collisions
    {ok, UserLine} = wait(Socket, "USER "),
    UserTokens = string:tokens(UserLine, " \n"),
    Username = lists:nth(2, UserTokens),
    ClientHost = lists:nth(3, UserTokens),
    ServerName = lists:nth(4, UserTokens),
    RealName = lists:nth(5, UserTokens),
    io:format("USER = ~p ~p ~p ~p~n", [Username, ClientHost, ServerName, RealName]),
    send(Socket, "PING :123456789\r\n"),
    wait(Socket),
    % TODO: check PING response
    send(Socket, ":" ++ ?SERVER_NAME ++ " 001 " ++ Nick ++ " :Welcome to " ++ ?SERVER_NAME ++ "\r\n"),
    send(Socket, ":" ++ ?SERVER_NAME ++ " 002 " ++ Nick ++ " :Powered by not_invented_here\r\n"),
    send(Socket, "PING :" ++ ?SERVER_NAME ++ "\r\n"),
    wait(Socket),
    % TODO: check PING response    
    send(Socket, ":" ++ ?SERVER_NAME ++ " 302 " ++ Nick ++ " :" ++ Nick ++ "=+~" ++ Username ++ "@" ++ ?SERVER_NAME ++ "\r\n"),
    {Nick, Username, ClientHost, ServerName, RealName}.

wait(Socket) ->
    case gen_tcp:recv(Socket, 0, 200) of
        {ok, _} ->
            wait(Socket);
        {error, _} ->
            ok
    end.
            
wait(Socket, Until) ->
    case gen_tcp:recv(Socket, 0, 200) of
        {ok, Packet} ->
            case string:equal(string:left(Packet, string:len(Until)), Until) of 
                true ->
                    {ok, Packet};
                false ->
                    wait(Socket, Until)
            end;
        {error, _} ->
            wait(Socket, Until)
    end.    
    
send(Socket, Data) ->
    io:format("<- ~p~n", [Data]),
    gen_tcp:send(Socket, Data).
