-module(utils_tests).
-include_lib("eunit/include/eunit.hrl").

simple_replace_test() ->
    Result = utils:replace(3, 9, [1, 2, 3, 4]),
    ?assert(lists:sort(Result) =:= [1, 2, 4, 9]).
    
missing_search_replace_test() ->
    Result = utils:replace(9, 3, [1, 2, 3, 4]),
    ?assert(lists:sort(Result) =:= [1, 2, 3, 4]).
    
multiple_match_replace_test() ->
    Result = utils:replace(3, 9, [1, 3, 2, 3, 4, 3, 5]),
    ?assert(lists:sort(Result) =:= [1, 2, 4, 5, 9, 9, 9]).

date_diff_test() ->
    {Days, Time} = utils:date_diff({{1, 1, 1}, {1, 1, 1}}, {{1, 1, 1}, {1, 2, 1}}),
    ?assert(Days =:= 0),
    ?assert(Time =:= {0, 1, 0}).
    
date_diff2_test() ->
    {Days, Time} = utils:date_diff({{1, 1, 1}, {1, 1, 1}}, {{1, 1, 1}, {3, 2, 30}}),
    ?assert(Days =:= 0),
    ?assert(Time =:= {2, 1, 29}).
    