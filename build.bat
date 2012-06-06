@echo off
if exist "%ProgramFiles(x86)%" "%ProgramFiles(x86)%\erl5.9\bin\erlc" not_invented_here.erl irc.erl client_handler.erl utils.erl
if not exist "%ProgramFiles(x86)%" "%ProgramFiles%\erl5.9\bin\erlc" not_invented_here.erl irc.erl client_handler.erl utils.erl
