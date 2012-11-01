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
