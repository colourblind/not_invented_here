-module(utils).

-export([get_server_prefix/1, get_server_command/1, get_server_params/1]).
-export([get_client_command/1, get_client_params/1]).

get_server_prefix(Message) ->
    hd(string:tokens(Message, " ")).
    
get_server_command(Message) ->
    hd(tl(string:tokens(Message, " "))).
    
get_server_params(Message) ->
    Params = string:join(tl(tl(string:tokens(Message, " "))), " "),
    reconstruct(string:tokens(Params, " ")).

get_client_command(Message) ->
    hd(string:tokens(Message, " ")).
    
get_client_params(Message) ->
    Params = string:join(tl(string:tokens(Message, " ")), " "),
    reconstruct(string:tokens(Params, " ")).

    
reconstruct([]) ->
    [];
reconstruct(ParamList) ->
    case hd(hd(ParamList)) of
        58 ->
            [tl(string:join(ParamList, " "))];
        _ ->
            [hd(ParamList) | reconstruct(tl(ParamList))]
    end.