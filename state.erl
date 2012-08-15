-module(state).
-behaviour(gen_server).

-include("records.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_user/2, get_channel/2, get_channels/1, get_channels_for_user/2]).
-export([add_user/2, remove_user/2, join_channel/3, part_channel/3]).
-export([change_nick/3]).
-export([start_link/0]).

get_user(Pid, Id) ->
    gen_server:call(Pid, {get_user, Id}).
           
get_channel(Pid, ChannelName) ->
    gen_server:call(Pid, {get_channel, ChannelName}).
    
get_channels(Pid) ->
    gen_server:call(Pid, {get_channels, false}).
    
get_channels_for_user(Pid, User) ->
    gen_server:call(Pid, {get_channels_for_user, User}).
    
add_user(Pid, User) ->
    gen_server:call(Pid, {add_user, User}).
    
remove_user(Pid, User) ->
    gen_server:call(Pid, {remove_user, User}).
    
join_channel(Pid, ChannelName, User) ->
    gen_server:call(Pid, {join_channel, ChannelName, User}).
    
part_channel(Pid, ChannelName, User) ->
    gen_server:call(Pid, {part_channel, ChannelName, User}).
    
change_nick(Pid, NewNick, User) ->
    gen_server:call(Pid, {change_nick, NewNick, User}).

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
handle_call({get_channels, _}, _, State) ->
    {reply, element(2, State), State};
handle_call({get_channels_for_user, User}, _, State) ->
    ChannelList = lists:filter(fun(Channel) -> lists:member(User#user.nick, Channel#channel.users) end, element(2, State)),
    {reply, ChannelList, State};
handle_call({add_user, User}, _, State) ->
    NewState = {lists:append(element(1, State), [User]), element(2, State)},
    {reply, NewState, NewState};
handle_call({remove_user, User}, _, State) ->
    NewState = {lists:delete(User, element(1, State)), element(2, State)},
    {reply, ok, NewState};
handle_call({join_channel, ChannelName, User}, _, State) ->
    io:format("Adding ~p to channel ~p~n", [User#user.nick, ChannelName]),
    Channel = lists:keyfind(ChannelName, 2, element(2, State)),
    case Channel of
        false ->
            io:format("Creating channel ~p~n", [ChannelName]),
            NewChan = #channel{name=ChannelName, topic="Test topic", users=[User#user.nick], mode="nt"}, 
            % set up user as chanop
            NewState = {element(1, State), [NewChan|element(2, State)]};
        _ ->
            NewChan = setelement(4, Channel, [User#user.nick|Channel#channel.users]),
            NewState = {element(1, State), lists:keyreplace(ChannelName, 2, element(2, State), NewChan)}
    end,
    {reply, NewChan, NewState};
handle_call({part_channel, ChannelName, User}, _, State) ->
    io:format("Removing ~p from channel ~p~n", [User#user.nick, ChannelName]),
    Channel = lists:keyfind(ChannelName, 2, element(2, State)),
    case Channel of
        false ->
            NewState = State,
            Result = false;
        _ ->
            NewChan = setelement(4, Channel, lists:delete(User#user.nick, Channel#channel.users)),
            case length(NewChan#channel.users) of
                0 ->
                    io:format("Removing dead channel: ~p~n", [ChannelName]),
                    NewState = {element(1, State), lists:keydelete(ChannelName, 2, element(2, State))};
                _ ->
                    NewState = {element(1, State), lists:keyreplace(ChannelName, 2, element(2, State), NewChan)}
            end,
            Result = ok
    end,
    {reply, Result, NewState};
handle_call({change_nick, NewNick, User}, _, State) ->
    NewUser = setelement(2, User, NewNick),
    NewUserList = lists:keyreplace(User#user.nick, 2, element(1, State), NewUser),
    NewChannelList = lists:map(fun(Channel) -> setelement(4, Channel, utils:replace(User#user.nick, NewNick, Channel#channel.users)) end, element(2, State)),
    {reply, ok, {NewUserList, NewChannelList}};
handle_call(Request, _, State) ->
    io:format("HANDLE_CALL: ~p~n", [Request]),
    {noreply, State}.
    
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
