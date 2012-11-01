@echo off
echo Building app . . .
call build
echo Copying binaries . . .
copy ebin\*.beam tests > nul

echo Building tests . . .
pushd tests
if exist "%ProgramFiles(x86)%" "%ProgramFiles(x86)%\erl5.9\bin\erlc" -I ..\include state_tests.erl utils_tests.erl
if not exist "%ProgramFiles(x86)%" "%ProgramFiles%\erl5.9\bin\erlc" -I ..\include state_tests.erl utils_tests.erl
popd

echo Running . . .

if exist "%ProgramFiles(x86)%" "%ProgramFiles(x86)%\erl5.9\bin\erl" -noshell -pa tests -s state_tests test -run init stop
if not exist "%ProgramFiles(x86)%" "%ProgramFiles%\erl5.9\bin\erl" -noshell -pa tests -s state_tests test -run init stop

if exist "%ProgramFiles(x86)%" "%ProgramFiles(x86)%\erl5.9\bin\erl" -noshell -pa tests -s eunit test utils_tests -run init stop
if not exist "%ProgramFiles(x86)%" "%ProgramFiles%\erl5.9\bin\erl" -noshell -pa tests -s eunit test utils_tests -run init stop