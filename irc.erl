-module(irc).
-behaviour(gen_server).

-include("config.hrl").
-include("records.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_message/4, join_channel/3, add_user/2, start_link/0]).

send_message(Pid, SenderPid, Recipient, Message) ->
    case Message of
        "EXPLODE" ->
            erlang:error("Because");
        _ ->
            pass
    end,

    Sender = get_user_by_pid(Pid, SenderPid),
    case hd(Recipient) of
        $# ->
            send_to_channel(Pid, Sender, Recipient, Message);
        $& ->
            send_to_channel(Pid, Sender, Recipient, Message);
        _ ->
            send_to_user(Pid, Sender, Recipient, Message)
    end.
    
send_to_user(Pid, Sender, RecipientNick, Message) ->
    case get_user_by_nick(Pid, RecipientNick) of
        false ->
            io:format("CANNOT FIND RECIPIENT ~p~n", [RecipientNick]);
        Recipient ->
            Prefix = ":" ++ Sender#user.nick ++ "!" ++ Sender#user.username ++ "@" ++ Sender#user.clientHost,
            FinalMessage = Prefix ++ " PRIVMSG " ++ Recipient#user.nick ++ " :" ++ Message ++ "\r\n",
            io:format("SENDING '~p'~n", [FinalMessage]),
            client_handler:send_message(Recipient#user.clientPid, FinalMessage)
    end.
    
send_to_channel(Pid, Sender, RecipientChannel, Message) ->
    case get_channel(Pid, RecipientChannel) of
        false ->
            io:format("CANNOT FIND RECIPIENT ~p~n", [RecipientChannel]);
        Channel ->
            Prefix = ":" ++ Sender#user.nick ++ "!" ++ Sender#user.username ++ "@" ++ Sender#user.clientHost,
            FinalMessage = Prefix ++ " PRIVMSG " ++ Channel#channel.name ++ " :" ++ Message ++ "\r\n",
            io:format("SENDING '~p'~n", [FinalMessage]),
            UserList = lists:delete(Sender, lists:map(fun(Nick) -> get_user_by_nick(Pid, Nick) end, Channel#channel.users)),
            lists:foreach(fun(User) -> client_handler:send_message(User#user.clientPid, FinalMessage) end, UserList)
    end.
    
join_channel(Pid, SenderPid, ChannelName) ->
    Sender = get_user_by_pid(Pid, SenderPid),
    gen_server:cast(Pid, {join_channel, ChannelName, Sender}).

get_user_by_nick(Pid, Nick) ->
    gen_server:call(Pid, {get_user, utils:normalise_nick(Nick)}).
           
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
    UserList = lists:map(fun(Nick) -> lists:keyfind(utils:normalise_nick(Nick), 2, element(1, State)) end, NewChan#channel.users),
    lists:foreach(fun(User) -> client_handler:send_message(User#user.clientPid, FinalMessage) end, UserList),
    client_handler:send_message(User#user.clientPid, ":" ++ ?SERVER_NAME ++ " 353 " ++ User#user.nick ++ " " ++ ChannelName ++ " :" ++ string:join(NewChan#channel.users, " ") ++ "\r\n"),
    client_handler:send_message(User#user.clientPid, ":" ++ ?SERVER_NAME ++ " 366 " ++ User#user.nick ++ " " ++ ChannelName ++ " :End of /NAMES list.\r\n"),
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
