-module(irc).
-behaviour(gen_server).

-include("records.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_to_user/4, add_user/2, start_link/0]).

send_to_user(Pid, SenderPid, RecipientNick, Message) ->
    Sender = get_user_by_pid(Pid, SenderPid),
    Recipient = get_user_by_nick(Pid, RecipientNick),
    Prefix = ":" ++ Sender#user.nick ++ "!" ++ Sender#user.username ++ "@" ++ Sender#user.clientHost,
    FinalMessage = Prefix ++ " PRIVMSG " ++ Recipient#user.nick ++ " :" ++ Message,
    io:format("SENDING '~p'~n", [FinalMessage]),
    client_handler:send_message(Recipient#user.clientPid, FinalMessage).

get_user_by_nick(Pid, Nick) ->
    gen_server:call(Pid, {get_user, Nick}).
get_user_by_pid(Pid, ClientPid) ->
    gen_server:call(Pid, {get_user, ClientPid}).

get_channel(Pid, Channel) ->
    gen_server:call(Pid, {get_channel, Channel}).
    
get_user_list(Pid, Channel) ->
    gen_server:call(Pid, {get_user_list, Channel}).
    
add_user(Pid, User) ->
    gen_server:call(Pid, {add_user, User}).


start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, {[], []}}.

handle_call({get_user, Pid}, _, State) when is_pid(Pid) ->
    User = lists:keyfind(Pid, 3, element(1, State)),
    {reply, User, State};
handle_call({get_user, Nick}, _, State) ->
    User = lists:keyfind(Nick, 2, element(1, State)),
    {reply, User, State};

handle_call({get_channel, Channel}, _, State) ->
    {reply, "Here is your channel!", State};
handle_call({get_user_list, Channel}, _, State) ->
    {reply, element(1, State), State};
handle_call({add_user, User}, _, State) ->
    io:format("ADD USER ~p~n", [User]),
    NewState = {lists:append(element(1, State), [User]), element(2, State)},
    {reply, NewState, NewState};
handle_call(Request, {Pid, Tag}, State) ->
    {noreply, State}.
    
handle_cast(Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
