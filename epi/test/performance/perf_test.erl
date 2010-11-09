% ./epi_perf_tests pepito@makako.local pepita@makako.local one_cookie 1

-module(perf_test).
-export([do_test/2, test1/0, test2/0, test3/0, test4/1]).

millis() ->
	{_, Secs, MicroSecs} = now(),
	Secs*1000+MicroSecs/1000.
	
do_test2(0, _) -> ok;
do_test2(Times, F) -> 
	F(),
	do_test2(Times-1, F).
	
do_test(Times, F) ->
	Ini = millis(),
	do_test2(Times, F),
	millis()-Ini.
	
test1() ->
	F = fun() ->
		{perf,  'pepito@makako.local'}!{self(), 1},
		receive X -> X end
		end,
	perf_test:do_test(10000, F).

test2() ->
	F = fun() ->
		{perf,  'pepito@makako.local'}!{self(), "hola", "mundo"},
		receive X -> X end
		end,
	perf_test:do_test(10000, F).

test3() ->
	F = fun() ->
		{perf,  'pepito@makako.local'}!{incr, self(), 1},
		receive X -> X end,
		{perf,  'pepito@makako.local'}!{concat, self(), "hola", "mundo"},
		receive Y -> Y end
		end,
	perf_test:do_test(5000, F).

sequence(0)->[0];
sequence(N)->[N|sequence(N-1)].

test4(N) ->
	Numbers = sequence(N),
	F = fun() ->
		{perf,  'pepito@makako.local'}!{self(), Numbers},
		receive Y -> Y end
		end,
	perf_test:do_test(5000, F).
	
	
% {perf,  'pepito@makako.local'}!{incr, self(), 1}.

% perf_test:test1().
% perf_test:test2().
% perf_test:test3().
% perf_test:test4().
% lists:map(fun(X) -> perf_test:test4(X) end, [5, 10, 20, 50, 100]).
% net_adm:ping('pepito@makako.local').

