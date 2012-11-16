-module(utils).

-include("config.hrl").
-include("records.hrl").

-export([get_server_prefix/1, get_server_command/1, get_server_params/1]).
-export([get_client_command/1, get_client_params/1]).
-export([get_user_prefix/1, resolve_mode/2, fix_nick/3, err_msg/3]).
-export([replace/3, date_diff/2]).

get_server_prefix(Message) ->
    hd(string:tokens(Message, " ")).
    
get_server_command(Message) ->
    hd(tl(string:tokens(Message, " "))).
    
get_server_params(Message) ->
    Trimmed = string:strip(string:strip(Message, right, $\n), right, $\r),
    Params = string:join(tl(tl(string:tokens(Trimmed, " "))), " "),
    reconstruct(string:tokens(Params, " ")).

get_client_command(Message) ->
    Trimmed = string:strip(string:strip(Message, right, $\n), right, $\r),
    hd(string:tokens(Trimmed, " ")).
    
get_client_params(Message) ->
    Trimmed = string:strip(string:strip(Message, right, $\n), right, $\r),
    Params = string:join(tl(string:tokens(Trimmed, " ")), " "),
    reconstruct(string:tokens(Params, " ")).
    
get_user_prefix(User) ->
    User#user.nick ++ "!" ++ User#user.username ++ "@" ++ User#user.clientHost.

resolve_mode(ModeString, NewModes) ->
    case hd(NewModes) of
        $+ ->
            Result = lists:append(ModeString, tl(NewModes));
        $- ->
            Result = lists:subtract(ModeString, tl(NewModes))
    end,
    lists:usort(Result).
    
fix_nick(User, OpList, VoiceList) ->
    case lists:member(User#user.clientPid, OpList) of
        true ->
            "@" ++ User#user.nick;
        false ->
            case lists:member(User#user.clientPid, VoiceList) of
                true ->
                    "+" ++ User#user.nick;
                false ->
                    User#user.nick
            end
    end.
    
err_msg(nosuchnick, Sender, ChannelName) ->
    ":" ++ ?SERVER_NAME ++ " 401 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :No such nick/channel\r\n";
err_msg(nosuchchannel, Sender, ChannelName) ->
    ":" ++ ?SERVER_NAME ++ " 403 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :No such channel\r\n";
err_msg(cannotsendtochan, Sender, ChannelName) ->
    ":" ++ ?SERVER_NAME ++ " 404 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :Cannot send to channel\r\n";
err_msg(unknowncommand, Sender, Command) ->
    ":" ++ ?SERVER_NAME ++ " 421 " ++ Sender#user.nick ++ " " ++ Command ++ " :Unknown command\r\n";
err_msg(nicknameinuse, Sender, Nick) ->
    ":" ++ ?SERVER_NAME ++ " 433 " ++ Sender#user.nick ++ " " ++ Nick ++ " Nickname is already in use\r\n";
err_msg(chanoprivsneeded, Sender, ChannelName) ->
    ":" ++ ?SERVER_NAME ++ " 482 " ++ Sender#user.nick ++ " " ++ ChannelName ++ " :You're not channel operator\r\n".

reconstruct([]) ->
    [];
reconstruct(ParamList) ->
    case hd(hd(ParamList)) of
        58 ->
            [tl(string:join(ParamList, " "))];
        _ ->
            [hd(ParamList) | reconstruct(tl(ParamList))]
    end.
    
replace(Search, Replace, List) -> replace(Search, Replace, List, []).

replace(_Search, _Replace, [], NewList) -> NewList;
replace(Search, Replace, [Search|OldTail], NewList) -> replace(Search, Replace, OldTail, [Replace|NewList]);
replace(Search, Replace, [NonMatch|OldTail], NewList) -> replace(Search, Replace, OldTail, [NonMatch|NewList]).

date_diff(T1, T2) ->
    S1 = calendar:datetime_to_gregorian_seconds(T1),
    S2 = calendar:datetime_to_gregorian_seconds(T2),
    calendar:seconds_to_daystime(S2 - S1).
