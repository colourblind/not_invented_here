-module(handshake).
-bahaviour(gen_fsm).

-include("records.hrl").

-export([start_link/2]).
-export([init/1, handle_event/3, handle_sync_event/4, code_change/4, terminate/3]).
-export([start/2, nick/2, user/2]).

start_link(ServerPid, Socket) ->
    io:format("STARTING HANDSHAKE~n"),
    {ok, FsmPid} = gen_fsm:start_link(?MODULE, {self(), Socket, ServerPid}, []),
    gen_tcp:controlling_process(Socket, self()),
    loop(FsmPid).
    
loop(FsmPid) ->
    receive
        {tcp, _, Packet} ->
            io:format("handshake packet recv: ~p~n", [Packet]),
            Params = utils:get_client_params(Packet),
            case utils:get_client_command(Packet) of 
                "CAP" ->
                    % Ignoring for now . . .
                    loop(FsmPid);
                "NICK" ->
                    gen_fsm:send_event(FsmPid, {nick, hd(Params)}),
                    loop(FsmPid);
                "USER" ->
                    gen_fsm:send_event(FsmPid, {user, Packet}),
                    loop(FsmPid);
                _ ->
                    io:format("Unknown handshake packet! ~p~n", [Packet])
            end;
        {stop, {ClientPid, Socket, Nick}} ->
            case gen_tcp:controlling_process(Socket, ClientPid) of
                ok ->
                    io:format("Control for socket ~p transferred to ~p~n", [Socket, ClientPid]);
                {error, Reason} ->
                    io:format("Control for socket ~p transfer failed! ~p~n", [Socket, Reason])
            end,
            gen_tcp:send(Socket, ":" ++ cfg:server_name() ++ " 001 " ++ Nick ++ " :Welcome to " ++ cfg:server_name() ++ "\r\n"),
            gen_tcp:send(Socket, ":" ++ cfg:server_name() ++ " 002 " ++ Nick ++ " :Powered by not_invented_here\r\n"),
            gen_tcp:send(Socket, ":" ++ cfg:server_name() ++ " 003 " ++ Nick ++ " :Y U NO WERK?!\r\n"),
            ok;
        Wut ->
            io:format("Message received: ~p~n", [Wut])
    end.

start({nick, Nick}, {ParentPid, Socket, ServerPid}) ->
    % Make sure the nick is available
    case state:get_user(ServerPid, Nick) of
        false ->
            {next_state, nick, {ParentPid, Socket, ServerPid, Nick}};
        _ ->
            FinalMessage = ":" ++ cfg:server_name() ++ " 433 " ++ Nick ++ " Nickname is already in use\r\n",
            gen_tcp:send(Socket, FinalMessage),
            {next_state, start, {ParentPid, Socket, ServerPid}}
    end;
start({user, UserLine}, {ParentPid, Socket, ServerPid}) ->
    UserTokens = string:tokens(UserLine, " \n"),
    Username = lists:nth(2, UserTokens),
    % ClientHost = lists:nth(3, UserTokens),
    ServerName = lists:nth(4, UserTokens),
    RealName = lists:nth(5, UserTokens),
    
    {ok, {Ip, _}} = inet:peername(Socket),
    ClientHost = inet_parse:ntoa(Ip),
    {next_state, user, {ParentPid, Socket, ServerPid, "", Username, ClientHost, ServerName, RealName}}.


nick({user, UserLine}, {ParentPid, Socket, ServerPid, Nick}) ->
    UserTokens = string:tokens(UserLine, " \n"),
    Username = lists:nth(2, UserTokens),
    % ClientHost = lists:nth(3, UserTokens),
    ServerName = lists:nth(4, UserTokens),
    RealName = lists:nth(5, UserTokens),
    
    {ok, {Ip, _}} = inet:peername(Socket),
    ClientHost = inet_parse:ntoa(Ip),

    finish({ParentPid, Socket, ServerPid, Nick, Username, ClientHost, ServerName, RealName}).
    
user({nick, Nick}, {ParentPid, Socket, ServerPid, _, Username, ClientHost, ServerName, RealName}) ->
    % Make sure the nick is available
    case state:get_user(ServerPid, Nick) of
        false ->
            finish({ParentPid, Socket, ServerPid, Nick, Username, ClientHost, ServerName, RealName});
        _ ->
            FinalMessage = ":" ++ cfg:server_name() ++ " 433 " ++ Nick ++ " Nickname is already in use\r\n",
            gen_tcp:send(Socket, FinalMessage),
            {next_state, user, {ParentPid, Socket, ServerPid, "", Username, ClientHost, ServerName, RealName}}
    end.
    
finish({ParentPid, Socket, ServerPid, Nick, Username, ClientHost, ServerName, RealName}) ->
    % We're spawning the client handlers via the state service so they'll die if it falls over.
    ClientPid = state:spawn_handler(ServerPid, Socket),
    state:add_user(ServerPid, #user{nick=Nick, clientPid=ClientPid, username=Username, clientHost=ClientHost, serverName=ServerName, realName=RealName, lastActivityTime=erlang:localtime()}),
    io:format("Created client ~p~n", [ClientPid]),
    % Chance of losing messages before we switch controlling process? :-/
    ParentPid ! {stop, {ClientPid, Socket, Nick}},
    {stop, normal, {ParentPid, Socket, ServerPid, Nick, Username, ClientHost, ServerName, RealName}}.
    
init(ServerPid) ->
    {ok, start, ServerPid}.

handle_event(Event, StateName, StateData) ->
    io:format("handle_event handshake ~p at ~p with state ~p~n", [Event, StateName, StateData]),
    {stop, wtf, StateData}.
    
handle_sync_event(Event, From, StateName, StateData) ->
    io:format("handle_event handshake ~p from ~p at ~p with state ~p~n", [Event, From, StateName, StateData]),
    {stop, wtf, StateData}.

code_change(OldVersion, StateName, StateData, Extra) ->
    io:format("code_change ~p ~p with state ~p (~p)~n", [OldVersion, StateName, StateData, Extra]),
    {ok, StateName, StateData}.
    
terminate(normal, user, _) ->
    ok;
terminate(Reason, StateName, StateData) ->
    io:format("terminating handshake ~p at ~p with state ~p~n", [Reason, StateName, StateData]),
    ok.
