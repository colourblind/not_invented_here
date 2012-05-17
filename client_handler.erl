-module(client_handler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2]).
    
start_link(ServerPid, Socket) ->
    gen_server:start_link(?MODULE, {ServerPid, Socket}, []).

init({ServerPid, Socket}) ->
    io:format("Initialising client_handler!~nState: ~p~nSocket ~p~n", [ServerPid, Socket]),
    {ok, {ServerPid, Socket}}.


handle_call(Request, {Pid, Tag}, State) ->
    io:format("handle_call ~p from ~p ~p with state ~p~n", [Request, Pid, Tag, State]),
    {noreply, State}.

handle_cast(Request, State) ->
    io:format("handle_cast ~p with state ~p~n", [Request, State]),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    io:format("handle_info TCP ~p ~p~n", [Socket, Data]),
    {noreply, State};
handle_info(Info, State) ->
    io:format("handle_info ~p with state ~p~n", [Info, State]),
    {noreply, State}.

terminate(Reason, State) ->
    io:format("terminate ~p with state ~p~n", [Reason, State]),
    ok.

code_change(OldVsn, State, Extra) ->
    io:format("code_change ~p ~p with state ~p~n", [OldVsn, Extra, State]),
    {ok, State}.
    