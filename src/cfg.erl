-module(cfg).

-export([listen_port/0, server_name/0, ping_interval/0, ping_timeout/0]).
-export([test/0]).

listen_port() ->
    {ok, Val} = application:get_env(listen_port),
    Val.

server_name() ->
    {ok, Val} = application:get_env(server_name),
    Val.
    
ping_interval() ->
    {ok, Val} = application:get_env(ping_interval),
    Val.

ping_timeout() ->
    {ok, Val} = application:get_env(ping_timeout),
    Val.

test() ->
    io:format("ALL_CONFIG~n~p~nDONE~n", [application:get_all_env()]).
