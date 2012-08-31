@echo off
pushd ebin
if exist "%ProgramFiles(x86)%" "%ProgramFiles(x86)%\erl5.9\bin\erl" -run not_invented_here start
if not exist "%ProgramFiles(x86)%" "%ProgramFiles%\erl5.9\bin\erl" -run not_invented_here start
popd
