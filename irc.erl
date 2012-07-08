-module(irc).

-include("config.hrl").
-include("records.hrl").

-export([send_message/4, join_channel/3, part_channel/3, mode/3, topic/3, quit/3, nick/3]).

send_message(Pid, SenderPid, Recipient, Message) ->
    case Message of
        "EXPLODE" ->
            erlang:error("Because");
        _ ->
            pass
    end,
    Sender = state:get_user(Pid, SenderPid),
    case hd(Recipient) of
        $# ->
            send_to_channel(Pid, Sender, Recipient, Message);
        $& ->
            send_to_channel(Pid, Sender, Recipient, Message);
        _ ->
            send_to_user(Pid, Sender, Recipient, Message)
    end.
    
send_to_user(Pid, Sender, RecipientNick, Message) ->
    case state:get_user(Pid, RecipientNick) of
        false ->
            io:format("CANNOT FIND RECIPIENT ~p~n", [RecipientNick]);
        Recipient ->
            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " PRIVMSG " ++ Recipient#user.nick ++ " :" ++ Message ++ "\r\n",
            io:format("SENDING '~p'~n", [FinalMessage]),
            client_handler:send_message(Recipient#user.clientPid, FinalMessage)
    end.
    
send_to_channel(Pid, Sender, RecipientChannel, Message) ->
    case state:get_channel(Pid, RecipientChannel) of
        false ->
            io:format("CANNOT FIND RECIPIENT ~p~n", [RecipientChannel]);
        Channel ->
            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " PRIVMSG " ++ Channel#channel.name ++ " :" ++ Message ++ "\r\n",
            io:format("SENDING '~p'~n", [FinalMessage]),
            UserList = lists:delete(Sender, lists:map(fun(Nick) -> state:get_user(Pid, Nick) end, Channel#channel.users)),
            lists:foreach(fun(User) -> client_handler:send_message(User#user.clientPid, FinalMessage) end, UserList)
    end.
    
send_raw_to_channel(Pid, RecipientChannel, Message) ->
    case state:get_channel(Pid, RecipientChannel) of
        false ->
            io:format("CANNOT FIND RECIPIENT ~p~n", [RecipientChannel]);
        Channel ->
            UserList = lists:map(fun(Nick) -> state:get_user(Pid, Nick) end, Channel#channel.users),
            lists:foreach(fun(User) -> client_handler:send_message(User#user.clientPid, Message) end, UserList)
    end.
    
join_channel(Pid, SenderPid, ChannelName) ->
    Sender = state:get_user(Pid, SenderPid),
    Channel = state:join_channel(Pid, ChannelName, Sender),
    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " JOIN " ++ " :"  ++ Channel#channel.name ++ "\r\n",
    send_raw_to_channel(Pid, ChannelName, FinalMessage),
    topic(Pid, SenderPid, ChannelName),
    client_handler:send_message(Sender#user.clientPid, ":" ++ ?SERVER_NAME ++ " 353 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :" ++ string:join(Channel#channel.users, " ") ++ "\r\n"),
    client_handler:send_message(Sender#user.clientPid, ":" ++ ?SERVER_NAME ++ " 366 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :End of /NAMES list.\r\n"),
    ok.

part_channel(Pid, SenderPid, ChannelName) ->
    Sender = state:get_user(Pid, SenderPid),
    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " PART " ++ " :"  ++ ChannelName ++ "\r\n",
    send_raw_to_channel(Pid, ChannelName, FinalMessage),
    case state:part_channel(Pid, ChannelName, Sender) of
        false ->
            io:format("Attempt to leave unknown channel: ~p ~p~n", [Sender#user.nick, ChannelName]);
        ok ->
            ok
    end.
    
mode(Pid, SenderPid, ChannelName) ->
    Sender = state:get_user(Pid, SenderPid),
    Channel = state:get_channel(Pid, ChannelName),
    FinalMessage = ":" ++ ?SERVER_NAME ++ " 324 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " +" ++ Channel#channel.mode ++ "\r\n",
    io:format("~p~n", [FinalMessage]),
    client_handler:send_message(SenderPid, FinalMessage).
    
topic(Pid, SenderPid, ChannelName) ->
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, ChannelName) of 
        false ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 403 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :No such channel\r\n";
        Channel ->
            case Channel#channel.topic of
                "" ->
                    FinalMessage = ":" ++ ?SERVER_NAME ++ " 331 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :No topic is set\r\n";
                T ->
                    FinalMessage = ":" ++ ?SERVER_NAME ++ " 332 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :" ++ T ++ "\r\n"
            end
    end,
    client_handler:send_message(SenderPid, FinalMessage).

quit(Pid, SenderPid, Reason) ->
    Sender = state:get_user(Pid, SenderPid),
    ChannelList = state:get_channels_for_user(Pid, Sender),
    lists:foreach(fun(Channel) -> state:part_channel(Pid, Channel#channel.name, Sender) end, ChannelList),
    state:remove_user(Pid, Sender),
    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " QUIT " ++ " :"  ++ Reason ++ "\r\n",
    lists:foreach(fun(Channel) -> send_raw_to_channel(Pid, Channel#channel.name, FinalMessage) end, ChannelList).
    
nick(Pid, SenderPid, NewNick) ->
    Sender = state:get_user(Pid, SenderPid),
    case state:get_user(Pid, NewNick) of
        false ->
            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " NICK " ++ " :"  ++ NewNick ++ "\r\n",
            state:change_nick(Pid, NewNick, Sender),
            client_handler:send_message(SenderPid, FinalMessage),
            ChannelList = state:get_channels_for_user(Pid, Sender),
            lists:foreach(fun(Channel) -> send_raw_to_channel(Pid, Channel#channel.name, FinalMessage) end, ChannelList);
            % Actually update user nick and channel nicks (DUPES IF USERS IN MULTIPLE CHANNELS WITH SENDER?)
        _ ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 433 " ++ Sender#user.nick ++ " Nickname is already in use\r\n",
            client_handler:send_message(SenderPid, FinalMessage)
    end.
            
    
    

