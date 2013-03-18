-module(irc).

-include("records.hrl").

-export([privmsg/4, unknown/3, join/3, part/3, names/3, list/2, mode/3, topic/3, quit/3, nick/3, userhost/3]).
-export([kick/3, whois/3, update_last_activity_time/2]).

privmsg(Pid, SenderPid, Recipient, Message) ->
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
    client_handler:send_message(SenderPid, utils:err_msg(unknowncommand, Sender, Command)).
    
join(Pid, SenderPid, ChannelName) ->
    % TODO - check for valid channel name
    Sender = state:get_user(Pid, SenderPid),
    case user_can_join_channel(Pid, Sender, ChannelName) of
        false ->
            client_handler:send_message(Sender#user.clientPid, utils:err_msg(bannedfromchan, Sender, ChannelName));
        true ->
            Channel = state:join_channel(Pid, ChannelName, Sender),
            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " JOIN " ++ " :"  ++ Channel#channel.name ++ "\r\n",
            send_raw_to_channel(Pid, ChannelName, FinalMessage),
            topic(Pid, SenderPid, [ChannelName]),
            names(Pid, SenderPid, ChannelName)
    end.

part(Pid, SenderPid, ChannelName) ->
    Sender = state:get_user(Pid, SenderPid),
    case state:part_channel(Pid, ChannelName, Sender) of
        false ->
            client_handler:send_message(SenderPid, utils:err_msg(nosuchchannel, Sender, ChannelName));
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
            NickList = lists:map(fun(User) -> utils:fix_nick(User, Channel#channel.ops, Channel#channel.voices) end, UserList),
            client_handler:send_message(Sender#user.clientPid, ":" ++ cfg:server_name() ++ " 353 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :" ++ string:join(NickList, " ") ++ "\r\n")
    end,
    client_handler:send_message(Sender#user.clientPid, ":" ++ cfg:server_name() ++ " 366 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :End of /NAMES list.\r\n").
    
list(Pid, SenderPid) ->
    Sender = state:get_user(Pid, SenderPid),
    ChannelList = state:get_channels(Pid),
    io:format("~p~n", [ChannelList]),
    client_handler:send_message(Sender#user.clientPid, ":" ++ cfg:server_name() ++ " 321 " ++ Sender#user.nick ++ " Channel :Users  Name\r\n"),
    lists:foreach(fun(Channel) -> client_handler:send_message(Sender#user.clientPid, ":" ++ cfg:server_name() ++ " 322 " ++ Sender#user.nick ++ " " ++ Channel#channel.name ++ " " ++ integer_to_list(length(Channel#channel.users)) ++ " :" ++ Channel#channel.topic ++ "\r\n") end, ChannelList),
    client_handler:send_message(Sender#user.clientPid, ":" ++ cfg:server_name() ++ " 323 " ++ Sender#user.nick ++ " :End of /LIST\r\n").

mode(Pid, SenderPid, Params) when length(Params) == 1 ->
    ChannelName = hd(Params),
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, ChannelName) of
        false ->
            client_handler:send_message(SenderPid, utils:err_msg(nosuchchannel, Sender, hd(Params)));
        Channel ->
            FinalMessage = ":" ++ cfg:server_name() ++ " 324 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " +" ++ Channel#channel.mode ++ "\r\n",
            client_handler:send_message(SenderPid, FinalMessage)
    end;
mode(Pid, SenderPid, [ChannelName, "+b"]) ->
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, ChannelName) of
        false ->
            client_handler:send_message(SenderPid, utils:err_msg(nosuchchannel, Sender, ChannelName));
        Channel ->
            lists:foreach(fun(Banmask) -> client_handler:send_message(Sender#user.clientPid, ":" ++ cfg:server_name() ++ " 367 " ++ Sender#user.nick ++ " " ++ Channel#channel.name ++ " " ++ Banmask ++ "\r\n") end, Channel#channel.bans),
            client_handler:send_message(Sender#user.clientPid, ":" ++ cfg:server_name() ++ " 368 " ++ Sender#user.nick ++ " " ++ Channel#channel.name ++ " :End of channel ban list\r\n")
    end;
mode(Pid, SenderPid, Params) when length(Params) == 2 ->
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, hd(Params)) of
        false ->
            client_handler:send_message(SenderPid, utils:err_msg(nosuchchannel, Sender, hd(Params)));
        Channel ->
            case lists:member(SenderPid, Channel#channel.ops) of
                false ->
                    client_handler:send_message(Sender#user.clientPid, utils:err_msg(chanoprivsneeded, Sender, Channel#channel.name));
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
            client_handler:send_message(SenderPid, utils:err_msg(nosuchchannel, Sender, hd(Params)));
        Channel ->
            case lists:member(SenderPid, Channel#channel.ops) of
                true ->
                    Mode = hd(tl(Params)),
                    mode(Pid, Sender, lists:nth(2, Mode), Channel, Params);
                false ->
                    client_handler:send_message(SenderPid, utils:err_msg(chanoprivsneeded, Sender, Channel#channel.name))
            end
    end.
mode(Pid, Sender, $o, Channel, Params) ->
    case state:get_user(Pid, lists:nth(3, Params)) of
        false ->
            client_handler:send_message(Sender#user.clientPid, utils:err_msg(nosuchnick, Sender, lists:nth(3, Params)));
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
    end;
mode(Pid, Sender, $v, Channel, Params) ->
    case state:get_user(Pid, lists:nth(3, Params)) of
        false ->
            client_handler:send_message(Sender#user.clientPid, utils:err_msg(nosuchnick, Sender, lists:nth(3, Params)));
        User ->
            case hd(lists:nth(2, Params)) of
                $+ ->
                    state:set_voice(Pid, Channel, User#user.clientPid),
                    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " MODE " ++ Channel#channel.name ++ " +v " ++ User#user.nick ++ "\r\n";
                $- ->
                    state:remove_voice(Pid, Channel, User#user.clientPid),
                    FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " MODE " ++ Channel#channel.name ++ " -v " ++ User#user.nick ++ "\r\n"
            end,
            send_raw_to_channel(Pid, Channel#channel.name, FinalMessage)
    end;
mode(Pid, Sender, $b, Channel, Params) ->
    Hostmask = lists:nth(3, Params),
    case hd(lists:nth(2, Params)) of
        $+ ->
            state:set_ban(Pid, Channel, Hostmask),
            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " MODE " ++ Channel#channel.name ++ " +b " ++ Hostmask ++ "\r\n";
        $- ->
            state:remove_ban(Pid, Channel, Hostmask),
            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " MODE " ++ Channel#channel.name ++ " -b " ++ Hostmask ++ "\r\n"
    end,
    send_raw_to_channel(Pid, Channel#channel.name, FinalMessage).
    
topic(Pid, SenderPid, Params) when length(Params) == 1 ->
    ChannelName = hd(Params),
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, ChannelName) of 
        false ->
            FinalMessage = utils:err_msg(nosuchchannel, Sender, ChannelName);
        Channel ->
            case Channel#channel.topic of
                "" ->
                    FinalMessage = ":" ++ cfg:server_name() ++ " 331 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :No topic is set\r\n";
                T ->
                    FinalMessage = ":" ++ cfg:server_name() ++ " 332 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :" ++ T ++ "\r\n"
            end
    end,
    client_handler:send_message(SenderPid, FinalMessage);
topic(Pid, SenderPid, Params) when length(Params) == 2 ->
    ChannelName = hd(Params),
    NewTopic = hd(tl(Params)),
    Sender = state:get_user(Pid, SenderPid),
    case state:get_channel(Pid, ChannelName) of
        false ->
            client_handler:send_message(SenderPid, utils:err_msg(nosuchchannel, Sender, ChannelName));
        Channel ->
            case lists:member($t, Channel#channel.mode) and not lists:member(Sender#user.clientPid, Channel#channel.ops) of
                true ->
                    client_handler:send_message(SenderPid, utils:err_msg(chanoprivsneeded, Sender, ChannelName));
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
            lists:foreach(fun(ClientPid) -> client_handler:send_message(ClientPid, FinalMessage) end, lists:usort([SenderPid|UserList]));
        _ ->
            client_handler:send_message(SenderPid, utils:err_msg(nicknameinuse, Sender, NewNick))
    end.

userhost(Pid, SenderPid, NewNick) ->
    Sender = state:get_user(Pid, SenderPid),
    case state:get_user(Pid, NewNick) of
        false ->
            % Dunno what to do here. RFC1459 doesn't say :-/
            ok;
        User ->
            FinalMessage = ":" ++ cfg:server_name() ++ " 302 " ++ Sender#user.nick ++ " :" ++ User#user.nick ++ "=+~" ++ User#user.username ++ "@" ++ User#user.clientHost ++ "\r\n",
            client_handler:send_message(SenderPid, FinalMessage)
    end.
    
kick(Pid, SenderPid, Params) ->
    Sender = state:get_user(Pid, SenderPid),
    ChannelName = hd(Params),
    case state:get_channel(Pid, hd(Params)) of
        false ->
            client_handler:send_message(SenderPid, utils:err_msg(nosuchchannel, Sender, ChannelName));
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
                    client_handler:send_message(SenderPid, utils:err_msg(chanoprivsneeded, Sender, ChannelName))
            end
    end.

whois(Pid, SenderPid, Nick) ->
    Sender = state:get_user(Pid, SenderPid),
    case state:get_user(Pid, Nick) of
        false ->
            client_handler:send_message(SenderPid, utils:err_msg(nosuchnick, Sender, Nick));
        User ->
            io:format("~p~n", [User]),
            client_handler:send_message(SenderPid, ":" ++ cfg:server_name() ++ " 311 " ++ Sender#user.nick ++ " " ++ User#user.nick ++ " " ++ User#user.username ++ " " ++ User#user.clientHost ++ " * :" ++ User#user.realName ++ "\r\n"),
            client_handler:send_message(SenderPid, ":" ++ cfg:server_name() ++ " 317 " ++ Sender#user.nick ++ " " ++ User#user.nick ++ " " ++ integer_to_list(utils:date_diff_seconds(User#user.lastActivityTime, erlang:localtime())) ++ " :seconds idle\r\n"),
            client_handler:send_message(SenderPid, ":" ++ cfg:server_name() ++ " 318 " ++ Sender#user.nick ++ " :End of /WHOIS list\r\n")
    end.
    
update_last_activity_time(Pid, SenderPid) ->
    Sender = state:get_user(Pid, SenderPid),
    state:update_last_activity_time(Pid, Sender).
    
% Helper functions
    
send_to_user(Pid, Sender, RecipientNick, Message) ->
    case state:get_user(Pid, RecipientNick) of
        false ->
            client_handler:send_message(Sender#user.clientPid, utils:err_msg(nosuchnick, Sender, RecipientNick));
        Recipient ->
            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " PRIVMSG " ++ Recipient#user.nick ++ " :" ++ Message ++ "\r\n",
            io:format("SENDING '~p'~n", [FinalMessage]),
            client_handler:send_message(Recipient#user.clientPid, FinalMessage)
    end.
    
send_to_channel(Pid, Sender, RecipientChannel, Message) ->
    case state:get_channel(Pid, RecipientChannel) of
        false ->
            client_handler:send_message(Sender#user.clientPid, utils:err_msg(nosuchnick, Sender, RecipientChannel));
        Channel ->
            case lists:member($n, Channel#channel.mode) and not lists:member(Sender#user.clientPid, Channel#channel.users) of
                true ->
                    client_handler:send_message(Sender#user.clientPid, utils:err_msg(cannotsendtochan, Sender, RecipientChannel));
                false ->
                    case lists:member($m, Channel#channel.mode) and not (lists:member(Sender#user.clientPid, Channel#channel.ops) or lists:member(Sender#user.clientPid, Channel#channel.voices)) of
                        true ->
                            client_handler:send_message(Sender#user.clientPid, utils:err_msg(cannotsendtochan, Sender, RecipientChannel));
                        false ->
                            FinalMessage = ":" ++ utils:get_user_prefix(Sender) ++ " PRIVMSG " ++ Channel#channel.name ++ " :" ++ Message ++ "\r\n",
                            io:format("SENDING '~p'~n", [FinalMessage]),
                            UserList = lists:delete(Sender#user.clientPid, Channel#channel.users),
                            lists:foreach(fun(ClientPid) -> client_handler:send_message(ClientPid, FinalMessage) end, UserList)
                    end
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
    
user_can_join_channel(Pid, Sender, ChannelName) ->
    case state:get_channel(Pid, ChannelName) of
        false ->
            true;
        Channel ->
            UserPrefix = utils:get_user_prefix(Sender),
            case lists:foldl(fun(X, Found) -> Found or utils:match_hostmask(UserPrefix, X) end, false, Channel#channel.bans) of
                true ->
                    false;
                false ->
                    true
            end
    end.

