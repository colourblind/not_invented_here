-module(utils).

-include("records.hrl").

-export([get_server_prefix/1, get_server_command/1, get_server_params/1]).
-export([get_client_command/1, get_client_params/1]).
-export([get_user_prefix/1]).
-export([replace/3]).

get_server_prefix(Message) ->
    hd(string:tokens(Message, " ")).
    
get_server_command(Message) ->
    hd(tl(string:tokens(Message, " "))).
    
get_server_params(Message) ->
    Trimmed = string:strip(Message, right, $\n),
    Params = string:join(tl(tl(string:tokens(Trimmed, " "))), " "),
    reconstruct(string:tokens(Params, " ")).

get_client_command(Message) ->
    Trimmed = string:strip(Message, right, $\n),
    hd(string:tokens(Trimmed, " ")).
    
get_client_params(Message) ->
    Trimmed = string:strip(Message, right, $\n),
    Params = string:join(tl(string:tokens(Trimmed, " ")), " "),
    reconstruct(string:tokens(Params, " ")).
    
get_user_prefix(User) ->
    User#user.nick ++ "!" ++ User#user.username ++ "@" ++ User#user.clientHost.

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
