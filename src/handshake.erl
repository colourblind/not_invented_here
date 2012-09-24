-module(handshake).
-bahaviour(gen_fsm).

-include("config.hrl").
-include("records.hrl").

-export([start_link/2]).
-export([init/1, handle_event/3, handle_sync_event/4, code_change/4, terminate/3]).
-export([nick/2, user/2]).

start_link(ServerPid, Socket) ->
    io:format("STARTING HANDSHAKE~n"),
    {ok, Pid} = gen_fsm:start_link(?MODULE, {Socket, ServerPid}, []),
    gen_tcp:controlling_process(Socket, Pid),
    loop(Pid, Socket).
    
loop(FsmPid, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            io:format("handshake packet recv: ~p~n", [Packet]),
            Params = utils:get_client_params(Packet),
            case utils:get_client_command(Packet) of 
                "CAP" ->
                    % Ignoring for now . . .
                    loop(FsmPid, Socket);
                "NICK" ->
                    gen_fsm:send_event(FsmPid, {nick, hd(Params)}),
                    loop(FsmPid, Socket);
                "USER" ->
                    gen_fsm:send_event(FsmPid, {user, Packet}),
                    loop(FsmPid, Socket);
                _ ->
                    io:format("Unknown handshake packet! ~p~n", [Packet])
            end;            
        {error, _} ->
            ok
    end.

nick({nick, Nick}, {Socket, ServerPid}) ->
    % Make sure the nick is available
    case state:get_user(ServerPid, Nick) of
        false ->
            {next_state, user, {Socket, ServerPid, Nick}};
        _ ->
            % TODO: respond to client
            {next_state, nick, {Socket, ServerPid}}
    end.
    
user({user, UserLine}, {Socket, ServerPid, Nick}) ->
    UserTokens = string:tokens(UserLine, " \n"),
    Username = lists:nth(2, UserTokens),
    % ClientHost = lists:nth(3, UserTokens),
    ServerName = lists:nth(4, UserTokens),
    RealName = lists:nth(5, UserTokens),
    
    {ok, {Ip, _}} = inet:peername(Socket),
    ClientHost = inet_parse:ntoa(Ip),
    % We're spawning the client handlers via the state service so they'll die if it falls over.
    Pid = state:spawn_handler(ServerPid, Socket),
    state:add_user(ServerPid, #user{nick=Nick, clientPid=Pid, username=Username, clientHost=ClientHost, serverName=ServerName, realName=RealName}),
    io:format("Created client ~p~n", [Pid]),
    % Chance of losing messages before we switch controlling process? :-/
    case gen_tcp:controlling_process(Socket, Pid) of
        ok ->
            io:format("Control for socket ~p transferred to ~p~n", [Socket, Pid]);
        {error, Reason} ->
            io:format("Control for socket ~p transfer failed! ~p~n", [Socket, Reason])
    end,
    gen_tcp:send(Socket, ":" ++ ?SERVER_NAME ++ " 001 " ++ Nick ++ " :Welcome to " ++ ?SERVER_NAME ++ "\r\n"),
    gen_tcp:send(Socket, ":" ++ ?SERVER_NAME ++ " 002 " ++ Nick ++ " :Powered by not_invented_here\r\n"),
    gen_tcp:send(Socket, ":" ++ ?SERVER_NAME ++ " 003 " ++ Nick ++ " :Y U NO WERK?!\r\n"),
    {stop, normal, {Socket, ServerPid, Nick, Username, ClientHost, ServerName, RealName}}.
    
init(ServerPid) ->
    {ok, nick, ServerPid}.

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
