-module(irc).

-include("config.hrl").
-include("records.hrl").

-export([send_message/4, unknown/3, join/3, part/3, names/3, list/2, mode/3, topic/3, quit/3, nick/3, userhost/3]).
-export([kick/3]).

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
            case lists:member($n, Channel#channel.mode) and not lists:member(Sender#user.clientPid, Channel#channel.users) of
                true ->
                    FinalMessage = ":" ++ ?SERVER_NAME ++ " 404 " ++ Sender#user.nick ++ " " ++ RecipientChannel ++ " :Cannot send to channel\r\n",
                    client_handler:send_message(Sender#user.clientPid, FinalMessage);
                false ->
                    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " PRIVMSG " ++ Channel#channel.name ++ " :" ++ Message ++ "\r\n",
                    io:format("SENDING '~p'~n", [FinalMessage]),
                    UserList = lists:delete(Sender#user.clientPid, Channel#channel.users),
                    lists:foreach(fun(ClientPid) -> client_handler:send_message(ClientPid, FinalMessage) end, UserList)
            end
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
    topic(Pid, SenderPid, [ChannelName]),
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
            NickList = lists:map(fun(User) -> utils:fix_nick(User, Channel#channel.ops) end, UserList),
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

mode(Pid, SenderPid, Params) when length(Params) == 1 ->
    ChannelName = hd(Params),
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, ChannelName) of
        false ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 403 " ++ Sender#user.nick ++ " " ++ hd(Params) ++ " :No such channel\r\n",
            client_handler:send_message(SenderPid, FinalMessage);
        Channel ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 324 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " +" ++ Channel#channel.mode ++ "\r\n",
            client_handler:send_message(SenderPid, FinalMessage)
    end;
mode(Pid, SenderPid, Params) when length(Params) == 2 ->
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, hd(Params)) of
        false ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 403 " ++ Sender#user.nick ++ " " ++ hd(Params) ++ " :No such channel\r\n",
            client_handler:send_message(SenderPid, FinalMessage);
        Channel ->
            case lists:member(SenderPid, Channel#channel.ops) of
                false ->
                    FinalMessage = ":" ++ ?SERVER_NAME ++ " 482 " ++ Sender#user.nick ++ " " ++ Channel#channel.name ++ " :You're not channel operator\r\n",
                    client_handler:send_message(Sender#user.clientPid, FinalMessage);
                true ->
                    Mode = hd(tl(Params)),
                    NewMode = utils:resolve_mode(Channel#channel.mode, Mode),
                    state:set_channel_mode(Pid, Channel#channel.name, NewMode),
                    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " MODE " ++ Channel#channel.name ++ " :" ++ Mode ++ "\r\n",
                    send_raw_to_channel(Pid, Channel#channel.name, FinalMessage)
            end
    end;
mode(Pid, SenderPid, Params) when length(Params) == 3 ->
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, hd(Params)) of
        false ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 403 " ++ Sender#user.nick ++ " " ++ hd(Params) ++ " :No such channel\r\n",
            client_handler:send_message(SenderPid, FinalMessage);
        Channel ->
            case lists:member(SenderPid, Channel#channel.ops) of
                true ->
                    Mode = hd(tl(Params)),
                    mode(Pid, Sender, lists:nth(2, Mode), Channel, Params);
                false ->
                    FinalMessage = ":" ++ ?SERVER_NAME ++ " 482 " ++ Sender#user.nick ++ " " ++ Channel#channel.name ++ " :You're not channel operator\r\n",
                    client_handler:send_message(Sender#user.clientPid, FinalMessage)
            end
    end.
mode(Pid, Sender, $o, Channel, Params) ->
    case state:get_user(Pid, lists:nth(3, Params)) of
        false ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 401 " ++ Sender#user.nick ++ " " ++ lists:nth(3, Params) ++ " :No such nick/channel\r\n",
            client_handler:send_message(Sender#user.clientPid, FinalMessage);
        User ->
            case hd(lists:nth(2, Params)) of
                $+ ->
                    state:set_chanop(Pid, Channel, User#user.clientPid),
                    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " MODE " ++ Channel#channel.name ++ " +o " ++ User#user.nick ++ "\r\n";
                $- ->
                    state:remove_chanop(Pid, Channel, User#user.clientPid),
                    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " MODE " ++ Channel#channel.name ++ " -o " ++ User#user.nick ++ "\r\n"
            end,
            send_raw_to_channel(Pid, Channel#channel.name, FinalMessage)
    end.
    
topic(Pid, SenderPid, Params) when length(Params) == 1 ->
    ChannelName = hd(Params),
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
    client_handler:send_message(SenderPid, FinalMessage);
topic(Pid, SenderPid, Params) when length(Params) == 2 ->
    ChannelName = hd(Params),
    NewTopic = hd(tl(Params)),
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, ChannelName) of
        false ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 403 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :No such channel\r\n",
            client_handler:send_message(SenderPid, FinalMessage);
        Channel ->
            case lists:member($t, Channel#channel.mode) and not lists:member(Sender#user.clientPid, Channel#channel.ops) of
                true ->
                    FinalMessage = ":" ++ ?SERVER_NAME ++ " 482 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :You're not channel operator\r\n",
                    client_handler:send_message(SenderPid, FinalMessage);
                false ->
                    state:set_topic(Pid, Channel, NewTopic),
                    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " TOPIC " ++ ChannelName ++ " :" ++ NewTopic ++ "\r\n",
                    send_raw_to_channel(Pid, ChannelName, FinalMessage)
            end
    end.

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

userhost(Pid, SenderPid, NewNick) ->
    Sender = state:get_user(Pid, SenderPid),
    case state:get_user(Pid, NewNick) of
        false ->
            % Dunno what to do here. RFC1459 doesn't say :-/
            ok;
        User ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 302 " ++ Sender#user.nick ++ " :" ++ User#user.nick ++ "=+~" ++ User#user.username ++ "@" ++ User#user.clientHost ++ "\r\n",
            client_handler:send_message(SenderPid, FinalMessage)
    end.
    
kick(Pid, SenderPid, Params) ->
    Sender = state:get_user(Pid, SenderPid),
    ChannelName = hd(Params),
    case state:get_channel(Pid, hd(Params)) of
        false ->
            FinalMessage = ":" ++ ?SERVER_NAME ++ " 403 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :No such channel\r\n",
            client_handler:send_message(SenderPid, FinalMessage);
        Channel ->
            case lists:member(Sender#user.clientPid, Channel#channel.ops) of
                true ->
                    case state:get_user(Pid, hd(tl(Params))) of
                        false ->
                            % TODO: nowwhat?
                            ok;
                        User ->
                            state:part_channel(Pid, Channel#channel.name, User),
                            % Todo message
                            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " KICK " ++ ChannelName ++ " :" ++ User#user.nick ++ "\r\n",
                            send_raw_to_channel(Pid, ChannelName, FinalMessage),
                            client_handler:send_message(User#user.clientPid, FinalMessage)
                    end;
                false ->
                    FinalMessage = ":" ++ ?SERVER_NAME ++ " 482 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :You're not channel operator\r\n",
                    client_handler:send_message(SenderPid, FinalMessage)
            end
    end.
