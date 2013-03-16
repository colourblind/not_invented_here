@echo off
if exist "%ProgramFiles(x86)%" "%ProgramFiles(x86)%\erl5.9\bin\erl" -make
if not exist "%ProgramFiles(x86)%" "%ProgramFiles%\erl5.9\bin\erl" -make