-module(client_handler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_message/2, start_link/2]).

send_message(Pid, Message) ->
    gen_server:cast(Pid, {irc, Message}).

start_link(ServerPid, Socket) ->
    gen_server:start_link(?MODULE, {ServerPid, Socket}, []).

init({ServerPid, Socket}) ->
    io:format("Initialising client_handler!~nState: ~p~nSocket ~p~n", [ServerPid, Socket]),
    erlang:send_after(cfg:ping_interval(), self(), send_ping),
    erlang:send_after(cfg:throttle_bleed_period(), self(), throttle_clear),
    % ServerPid, Socket, AwaitingPing, ThrottleQueue, Throttlevalue
    {ok, {ServerPid, Socket, false, [], 0}}.

handle_call(Request, {Pid, Tag}, State) ->
    io:format("handle_call ~p from ~p ~p with state ~p~n", [Request, Pid, Tag, State]),
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({irc, Message}, State) ->
    gen_tcp:send(element(2, State), Message),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    io:format("handle_info TCP ~p ~p~n", [Socket, Data]),
    Params = utils:get_client_params(Data),
    Command = utils:get_client_command(Data),
    NewValue = element(5, State) + 1,
    io:format("Setting throttle ~p~n", [NewValue]),
    case NewValue < cfg:throttle_threshold() of
        true ->
            {noreply, handle_command(Command, Params, setelement(5, State, NewValue))};
        false ->
            NewQueue = lists:append(element(4, State), [Data]),
            io:format("~p~n", [NewQueue]),
            {noreply, setelement(4, setelement(5, State, NewValue), NewQueue)}
    end;
handle_info(send_ping, State) ->
    gen_tcp:send(element(2, State), "PING :" ++ cfg:server_name() ++ "\r\n"),
    erlang:send_after(cfg:ping_timeout(), self(), ping_timeout),
    {noreply, setelement(3, State, true)};
handle_info(ping_timeout, State) ->
    case element(3, State) of
        true ->
            handle_command("QUIT", ["Ping timeout"], State),
            gen_tcp:close(element(2, State)),
            gen_server:cast(self(), stop);
        false ->
            ok
    end,
    {noreply, State};
handle_info(throttle_clear, State) ->
    io:format("Clearing throttle queue~n"),
    case element(5, State) of
        0 ->
            NewValue = 0;
        _ ->
            NewValue = element(5, State) - 1
    end,
    case length(element(4, State)) of
        0 ->
            FinalState = setelement(5, State, NewValue);
        _ ->
            [Message|NewQueue] = element(4, State),
            NewState = setelement(4, setelement(5, State, NewValue), NewQueue),
            Params = utils:get_client_params(Message),
            Command = utils:get_client_command(Message),
            FinalState = handle_command(Command, Params, NewState)
    end,
    erlang:send_after(cfg:throttle_bleed_period(), self(), throttle_clear),
    {noreply, FinalState};
handle_info({tcp_closed, _}, State) ->
    handle_command("QUIT", ["Connection reset by peer"], State),
    {stop, normal, State}.

terminate(Reason, State) ->
    io:format("terminate ~p with state ~p~n", [Reason, State]),
    ok.

code_change(OldVsn, State, Extra) ->
    io:format("code_change ~p ~p with state ~p~n", [OldVsn, Extra, State]),
    {ok, State}.
    
handle_command("PONG", _, State) ->
    erlang:send_after(cfg:ping_interval(), self(), send_ping),
    setelement(3, State, false);
handle_command("QUIT", Params, State) ->
    irc:quit(element(1, State), self(), lists:nth(1, Params)),
    gen_tcp:close(element(2, State)),
    gen_server:cast(self(), stop),
    State;
handle_command(Command, Params, State) ->
    case Command of
        "PRIVMSG" ->
            irc:privmsg(element(1, State), self(), lists:nth(1, Params), lists:nth(2, Params));
        "JOIN" ->
            irc:join(element(1, State), self(), lists:nth(1, Params));
        "PART" ->
            irc:part(element(1, State), self(), lists:nth(1, Params));
        "NAMES" ->
            irc:names(element(1, State), self(), lists:nth(1, Params));
        "LIST" ->
            irc:list(element(1, State), self());
        "MODE" ->
            irc:mode(element(1, State), self(), Params);
        "TOPIC" ->
            irc:topic(element(1, State), self(), Params);
        "NICK" ->
            irc:nick(element(1, State), self(), lists:nth(1, Params));
        "USERHOST" ->
            irc:userhost(element(1, State), self(), lists:nth(1, Params));
        "KICK" ->
            irc:kick(element(1, State), self(), Params);
        "WHOIS" ->
            irc:whois(element(1, State), self(), lists:nth(1, Params));
        "PING" ->
            io:format("Reponding to client PING~n"),
            gen_tcp:send(element(2, State), "PONG :" ++ lists:nth(1, Params) ++ "\r\n");
        Command ->
            irc:unknown(element(1, State), self(), Command)
    end,
    irc:update_last_activity_time(element(1, State), self()),
    State.
        