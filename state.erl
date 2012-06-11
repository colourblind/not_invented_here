-module(state).
-behaviour(gen_server).

-include("records.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_user_by_nick/2, get_user_by_pid/2, get_channel/2, get_user_list/2, add_user/2, join_channel/3, start_link/0]).

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
    
join_channel(Pid, ChannelName, User) ->
    gen_server:call(Pid, {join_channel, ChannelName, User}).


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
handle_call({join_channel, ChannelName, User}, _, State) ->
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
    {reply, NewChan, NewState};
handle_call(Request, {Pid, Tag}, State) ->
    io:format("HANDLE_CALL: ~p~n", Request),
    {noreply, State}.
    
handle_cast(Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
