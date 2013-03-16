@echo off
if exist "%ProgramFiles(x86)%" "%ProgramFiles(x86)%\erl5.9\bin\erl" -pa ebin -noshell -run not_invented_here no_really_start
if not exist "%ProgramFiles(x86)%" "%ProgramFiles%\erl5.9\bin\erl" -pa ebin -noshell -run not_invented_here no_really_start
