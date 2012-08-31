-module(irc).

-include("config.hrl").
-include("records.hrl").

-export([send_message/4, unknown/3, join/3, part/3, names/3, list/2, mode/3, topic/3, quit/3, nick/3]).

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
    
unknown(Pid, SenderPid, Command) ->
    % Would be nice to be able to deal with this directly in the client_handler,
    % but we don't have access to the user nick
    Sender = state:get_user(Pid, SenderPid),
    FinalMessage = ":" ++ ?SERVER_NAME ++ " 421 " ++ Sender#user.nick ++ " " ++ Command ++ " :Unknown command\r\n",
    client_handler:send_message(SenderPid, FinalMessage).
    
send_to_user(Pid, Sender, RecipientNick, Message) ->
    case state:get_user(Pid, RecipientNick) of
        false ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 401 " ++ Sender#user.nick ++ " " ++ RecipientNick ++ " :No such nick/channel\r\n",
            client_handler:send_message(Sender#user.clientPid, FinalMessage);
        Recipient ->
            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " PRIVMSG " ++ Recipient#user.nick ++ " :" ++ Message ++ "\r\n",
            io:format("SENDING '~p'~n", [FinalMessage]),
            client_handler:send_message(Recipient#user.clientPid, FinalMessage)
    end.
    
send_to_channel(Pid, Sender, RecipientChannel, Message) ->
    case state:get_channel(Pid, RecipientChannel) of
        false ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 401 " ++ Sender#user.nick ++ " " ++ RecipientChannel ++ " :No such nick/channel\r\n",
            client_handler:send_message(Sender#user.clientPid, FinalMessage);
        Channel ->
            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " PRIVMSG " ++ Channel#channel.name ++ " :" ++ Message ++ "\r\n",
            io:format("SENDING '~p'~n", [FinalMessage]),
            UserList = lists:delete(Sender, lists:map(fun(ClientPid) -> state:get_user(Pid, ClientPid) end, Channel#channel.users)),
            lists:foreach(fun(User) -> client_handler:send_message(User#user.clientPid, FinalMessage) end, UserList)
    end.
    
send_raw_to_channel(Pid, RecipientChannel, Message) ->
    case state:get_channel(Pid, RecipientChannel) of
        false ->
            io:format("CANNOT FIND RECIPIENT ~p~n", [RecipientChannel]);
        Channel ->
            UserList = lists:map(fun(ClientPid) -> state:get_user(Pid, ClientPid) end, Channel#channel.users),
            lists:foreach(fun(User) -> client_handler:send_message(User#user.clientPid, Message) end, UserList)
    end.
    
join(Pid, SenderPid, ChannelName) ->
    % TODO - check for valid channel name
    Sender = state:get_user(Pid, SenderPid),
    Channel = state:join_channel(Pid, ChannelName, Sender),
    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " JOIN " ++ " :"  ++ Channel#channel.name ++ "\r\n",
    send_raw_to_channel(Pid, ChannelName, FinalMessage),
    topic(Pid, SenderPid, ChannelName),
    names(Pid, SenderPid, ChannelName).

part(Pid, SenderPid, ChannelName) ->
    Sender = state:get_user(Pid, SenderPid),
    case state:part_channel(Pid, ChannelName, Sender) of
        false ->
            client_handler:send_message(SenderPid, ":" ++ ?SERVER_NAME ++ " 403 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :No such channel\r\n");
        ok ->
            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " PART " ++ " :"  ++ ChannelName ++ "\r\n",
            send_raw_to_channel(Pid, ChannelName, FinalMessage),
            client_handler:send_message(SenderPid, FinalMessage)
    end.
    
names(Pid, SenderPid, ChannelName) ->
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, ChannelName) of
        false ->
            ok;
        Channel ->
            UserList = lists:map(fun(ClientPid) -> state:get_user(Pid, ClientPid) end, Channel#channel.users),
            NickList = lists:map(fun(User) -> User#user.nick end, UserList),
            client_handler:send_message(Sender#user.clientPid, ":" ++ ?SERVER_NAME ++ " 353 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :" ++ string:join(NickList, " ") ++ "\r\n")
    end,
    client_handler:send_message(Sender#user.clientPid, ":" ++ ?SERVER_NAME ++ " 366 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :End of /NAMES list.\r\n").
    
list(Pid, SenderPid) ->
    Sender = state:get_user(Pid, SenderPid),
    ChannelList = state:get_channels(Pid),
    io:format("~p~n", [ChannelList]),
    client_handler:send_message(Sender#user.clientPid, ":" ++ ?SERVER_NAME ++ " 321 " ++ Sender#user.nick ++ " Channel :Users  Name\r\n"),
    lists:foreach(fun(Channel) -> client_handler:send_message(Sender#user.clientPid, ":" ++ ?SERVER_NAME ++ " 322 " ++ Sender#user.nick ++ " " ++ Channel#channel.name ++ " " ++ integer_to_list(length(Channel#channel.users)) ++ " :" ++ Channel#channel.topic ++ "\r\n") end, ChannelList),
    client_handler:send_message(Sender#user.clientPid, ":" ++ ?SERVER_NAME ++ " 323 " ++ Sender#user.nick ++ " :End of /LIST\r\n").

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
            ChannelList = state:get_channels_for_user(Pid, Sender),
            UserList = lists:foldl(fun(Channel, Acc) -> lists:append(Channel#channel.users, Acc) end, [], ChannelList),
            lists:foreach(fun(ClientPid) -> client_handler:send_message(ClientPid, FinalMessage) end, lists:usort(UserList));
        _ ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 433 " ++ Sender#user.nick ++ " Nickname is already in use\r\n",
            client_handler:send_message(SenderPid, FinalMessage)
    end.
            