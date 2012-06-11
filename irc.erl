-module(irc).

-include("config.hrl").
-include("records.hrl").

-export([send_message/4, join_channel/3]).

send_message(Pid, SenderPid, Recipient, Message) ->
    case Message of
        "EXPLODE" ->
            erlang:error("Because");
        _ ->
            pass
    end,
    Sender = state:get_user_by_pid(Pid, SenderPid),
    case hd(Recipient) of
        $# ->
            send_to_channel(Pid, Sender, Recipient, Message);
        $& ->
            send_to_channel(Pid, Sender, Recipient, Message);
        _ ->
            send_to_user(Pid, Sender, Recipient, Message)
    end.
    
send_to_user(Pid, Sender, RecipientNick, Message) ->
    case state:get_user_by_nick(Pid, RecipientNick) of
        false ->
            io:format("CANNOT FIND RECIPIENT ~p~n", [RecipientNick]);
        Recipient ->
            Prefix = ":" ++ Sender#user.nick ++ "!" ++ Sender#user.username ++ "@" ++ Sender#user.clientHost,
            FinalMessage = Prefix ++ " PRIVMSG " ++ Recipient#user.nick ++ " :" ++ Message ++ "\r\n",
            io:format("SENDING '~p'~n", [FinalMessage]),
            client_handler:send_message(Recipient#user.clientPid, FinalMessage)
    end.
    
send_to_channel(Pid, Sender, RecipientChannel, Message) ->
    case state:get_channel(Pid, RecipientChannel) of
        false ->
            io:format("CANNOT FIND RECIPIENT ~p~n", [RecipientChannel]);
        Channel ->
            Prefix = ":" ++ Sender#user.nick ++ "!" ++ Sender#user.username ++ "@" ++ Sender#user.clientHost,
            FinalMessage = Prefix ++ " PRIVMSG " ++ Channel#channel.name ++ " :" ++ Message ++ "\r\n",
            io:format("SENDING '~p'~n", [FinalMessage]),
            UserList = lists:delete(Sender, lists:map(fun(Nick) -> state:get_user_by_nick(Pid, Nick) end, Channel#channel.users)),
            lists:foreach(fun(User) -> client_handler:send_message(User#user.clientPid, FinalMessage) end, UserList)
    end.
    
send_raw_to_channel(Pid, RecipientChannel, Message) ->
    case state:get_channel(Pid, RecipientChannel) of
        false ->
            io:format("CANNOT FIND RECIPIENT ~p~n", [RecipientChannel]);
        Channel ->
            UserList = lists:map(fun(Nick) -> state:get_user_by_nick(Pid, Nick) end, Channel#channel.users),
            lists:foreach(fun(User) -> client_handler:send_message(User#user.clientPid, Message) end, UserList)
    end.
    
join_channel(Pid, SenderPid, ChannelName) ->
    Sender = state:get_user_by_pid(Pid, SenderPid),
    Channel = state:join_channel(Pid, ChannelName, Sender),
    Prefix = ":" ++ Sender#user.nick ++ "!" ++ Sender#user.username ++ "@" ++ Sender#user.clientHost,
    FinalMessage = Prefix ++ " JOIN " ++ " :"  ++ Channel#channel.name ++ "\r\n",
    send_raw_to_channel(Pid, ChannelName, FinalMessage),
    client_handler:send_message(Sender#user.clientPid, ":" ++ ?SERVER_NAME ++ " 353 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :" ++ string:join(Channel#channel.users, " ") ++ "\r\n"),
    client_handler:send_message(Sender#user.clientPid, ":" ++ ?SERVER_NAME ++ " 366 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :End of /NAMES list.\r\n"),
    ok.
