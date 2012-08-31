@echo off
pushd src
if exist "%ProgramFiles(x86)%" "%ProgramFiles(x86)%\erl5.9\bin\erlc" -o ..\ebin -I ..\include not_invented_here.erl irc.erl client_handler.erl utils.erl state.erl
if not exist "%ProgramFiles(x86)%" "%ProgramFiles%\erl5.9\bin\erlc" -o ..\ebin -I ..\include not_invented_here.erl irc.erl client_handler.erl utils.erl state.erl
popd