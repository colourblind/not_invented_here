-module(client_handler).
-behaviour(gen_server).

-include("config.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_message/2, start_link/2]).

send_message(Pid, Message) ->
    gen_server:cast(Pid, {irc, Message}).

start_link(ServerPid, Socket) ->
    gen_server:start_link(?MODULE, {ServerPid, Socket}, []).

init({ServerPid, Socket}) ->
    io:format("Initialising client_handler!~nState: ~p~nSocket ~p~n", [ServerPid, Socket]),
    erlang:send_after(?PING_INTERVAL, self(), send_ping),
    {ok, {ServerPid, Socket, false}}.

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
    {noreply, handle_command(Command, Params, State, Socket)};
handle_info(send_ping, State) ->
    gen_tcp:send(element(2, State), "PING :" ++ ?SERVER_NAME ++ "\r\n"),
    erlang:send_after(?PING_TIMEOUT, self(), ping_timeout),
    {noreply, setelement(3, State, true)};
handle_info(ping_timeout, State) ->
    case element(3, State) of
        true ->
            irc:quit(element(1, State), self(), "Ping timeout"),
            gen_tcp:close(element(2, State)),
            gen_server:cast(self(), stop);
        false ->
            ok
    end,
    {noreply, State}.

terminate(Reason, State) ->
    io:format("terminate ~p with state ~p~n", [Reason, State]),
    ok.

code_change(OldVsn, State, Extra) ->
    io:format("code_change ~p ~p with state ~p~n", [OldVsn, Extra, State]),
    {ok, State}.
    
handle_command("PONG", _, State, Socket) ->
    io:format("Received client PONG \\n ~p~n", [Socket]),
    erlang:send_after(?PING_INTERVAL, self(), send_ping),
    setelement(3, State, false);
handle_command(Command, Params, State, Socket) ->
    case Command of
        "PRIVMSG" ->
            irc:send_message(element(1, State), self(), lists:nth(1, Params), lists:nth(2, Params));
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
        "QUIT" ->
            irc:quit(element(1, State), self(), lists:nth(1, Params)),
            gen_tcp:close(Socket),
            gen_server:cast(self(), stop);
        "PING" ->
            io:format("Reponding to client PING~n"),
            gen_tcp:send(element(2, State), "PONG :" ++ lists:nth(1, Params) ++ "\r\n");
        Command ->
            irc:unknown(element(1, State), self(), Command)
    end,
    State.
        