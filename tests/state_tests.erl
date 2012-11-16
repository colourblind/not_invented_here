-module(state_tests).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

missing_user_test() ->
    {reply, Result, _} = state:handle_call({get_user, "whatevs"}, meh, {[], []}),
    ?assert(Result =:= false).
    
missing_user2_test() ->
    User = #user{nick="found", clientPid=self(), username="username", clientHost="clientHost", serverName="ServerName", realName="RealName"},
    {reply, Result, _} = state:handle_call({get_user, "found2"}, meh, {[User], []}),
    ?assert(Result =:= false).
    
found_user_test() ->
    User = #user{nick="found", clientPid=self(), username="username", clientHost="clientHost", serverName="ServerName", realName="RealName"},
    {reply, Result, _} = state:handle_call({get_user, "found"}, meh, {[User], []}),
    ?assert(Result#user.nick =:= "found").
    
add_chanop_test() ->
    Channel = #channel{name="#test", topic="", users=[], mode="", ops=[], voices=[]},
    User = #user{nick="found", clientPid=self(), username="username", clientHost="clientHost", serverName="ServerName", realName="RealName"},
    {reply, ok, {_, [Result]}} = state:handle_call({set_chanop, Channel, self()}, meh, {[User], [Channel]}),
    ?assert(lists:member(self(), Result#channel.ops)).

remove_chanop_test() ->
    User = #user{nick="found", clientPid=self(), username="username", clientHost="clientHost", serverName="ServerName", realName="RealName"},
    Channel = #channel{name="#test", topic="", users=[self()], mode="", ops=[self()], voices=[]},
    {reply, ok, {_, [Result]}} = state:handle_call({remove_chanop, Channel, self()}, meh, {[User], [Channel]}),
    ?assert(not lists:member(self(), Result#channel.ops)).
    
add_voice_test() ->
    Channel = #channel{name="#test", topic="", users=[], mode="", ops=[], voices=[]},
    User = #user{nick="found", clientPid=self(), username="username", clientHost="clientHost", serverName="ServerName", realName="RealName"},
    {reply, ok, {_, [Result]}} = state:handle_call({set_voice, Channel, self()}, meh, {[User], [Channel]}),
    ?assert(lists:member(self(), Result#channel.voices)).
    
remove_voice_test() ->
    User = #user{nick="found", clientPid=self(), username="username", clientHost="clientHost", serverName="ServerName", realName="RealName"},
    Channel = #channel{name="#test", topic="", users=[self()], mode="", ops=[], voices=[self()]},
    {reply, ok, {_, [Result]}} = state:handle_call({remove_voice, Channel, self()}, meh, {[User], [Channel]}),
    ?assert(not lists:member(self(), Result#channel.voices)).
