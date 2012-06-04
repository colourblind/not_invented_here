-module(irc).
-behaviour(gen_server).

-include("config.hrl").
-include("records.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_to_user/4, join_channel/3, send_to_channel/4, add_user/2, start_link/0]).

send_to_user(Pid, SenderPid, RecipientNick, Message) ->
    case Message of
        "EXPLODE" ->
            erlang:error("Because");
        _ ->
            pass
    end,

    Sender = get_user_by_pid(Pid, SenderPid),
    case get_user_by_nick(Pid, RecipientNick) of
        false ->
            io:format("CANNOT FIND RECIPIENT ~p~n", [RecipientNick]);
        Recipient ->
            Prefix = ":" ++ Sender#user.nick ++ "!" ++ Sender#user.username ++ "@" ++ Sender#user.clientHost,
            FinalMessage = Prefix ++ " PRIVMSG " ++ Recipient#user.nick ++ " :" ++ Message ++ "\r\n",
            io:format("SENDING '~p'~n", [FinalMessage]),
            client_handler:send_message(Recipient#user.clientPid, FinalMessage)
    end.
    
join_channel(Pid, SenderPid, ChannelName) ->
    Sender = get_user_by_pid(Pid, SenderPid),
    gen_server:cast(Pid, {join_channel, ChannelName, Sender}).
    
send_to_channel(Pid, SenderPid, Channel, Message) ->
    Sender = get_user_by_pid(Pid, SenderPid),
    case get_user_list(Pid, Channel) of
        false ->
            io:format("CANNOT FIND CHANNEL ~p~n", [Channel]);
        UserList ->
            Prefix = ":" ++ Sender#user.nick ++ "!" ++ Sender#user.username ++ "@" ++ Sender#user.clientHost,
            FinalMessage = Prefix ++ " PRIVMSG " ++ Channel ++ " :" ++ Message ++ "\r\n",
            lists:foreach(fun(User) -> client_handler:send_message(User#user.clientPid, FinalMessage) end, UserList)
    end.

get_user_by_nick(Pid, Nick) ->
    gen_server:call(Pid, {get_user, Nick}).
get_user_by_pid(Pid, ClientPid) ->
    gen_server:call(Pid, {get_user, ClientPid}).

get_channel(Pid, ChannelName) ->
    io:format("get_channel: ~p~n", [ChannelName]),
    gen_server:call(Pid, {get_channel, ChannelName}).
    
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

handle_call({get_channel, ChannelName}, _, State) ->
    Channel = lists:keyfind(ChannelName, 2, element(2, State)),
    {reply, Channel, State};
handle_call({get_user_list, Channel}, _, State) ->
    {reply, element(1, State), State};
handle_call({add_user, User}, _, State) ->
    io:format("ADD USER ~p~n", [User]),
    NewState = {lists:append(element(1, State), [User]), element(2, State)},
    {reply, NewState, NewState};
handle_call(Request, {Pid, Tag}, State) ->
    io:format("HANDLE_CALL: ~p~n", Request),
    {noreply, State}.
    
handle_cast({join_channel, ChannelName, User}, State) ->
    io:format("Adding ~p to channel ~p~n", [User#user.nick, ChannelName]),
    Channel = lists:keyfind(ChannelName, 2, element(2, State)),
    io:format("Channel: ~p~n", [Channel]),
    case Channel of
        false ->
            io:format("Creating channel ~p~n", [ChannelName]),
            NewChan = #channel{name=ChannelName, topic="Test topic", users=["@" ++ User#user.nick]},
            NewState = {element(1, State), [NewChan|element(2, State)]};
        _ ->
            NewChan = setelement(4, Channel, [User#user.nick|Channel#channel.users]),
            NewState = {element(1, State), lists:keyreplace(ChannelName, 2, element(2, State), NewChan)}
    end,
    Prefix = ":" ++ User#user.nick ++ "!" ++ User#user.username ++ "@" ++ User#user.clientHost,
    FinalMessage = Prefix ++ " JOIN " ++ " :"  ++ NewChan#channel.name ++ "\r\n",
    client_handler:send_message(User#user.clientPid, FinalMessage),
    client_handler:send_message(User#user.clientPid, ":" ++ ?SERVER_NAME ++ " 353 " ++ User#user.nick ++ " " ++ ChannelName ++ " :" ++ string:join(NewChan#channel.users, " ") ++ "\r\n"),
    client_handler:send_message(User#user.clientPid, ":" ++ ?SERVER_NAME ++ " 366 " ++ User#user.nick ++ " " ++ ChannelName ++ " :End of /NAMES list.\r\n"),
%    lists:foreach(fun(User) -> client_handler:send_message(User#user.clientPid, FinalMessage) end, UserList)
    io:format("~p~n", [NewState]),
    {noreply, NewState};
handle_cast(Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
