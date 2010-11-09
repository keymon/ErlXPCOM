-module(xpinterface_example).
-export([start/0, loop/0]).

start() ->
	Pid=spawn(?MODULE, loop, []),
	catch register(?MODULE, Pid),
    Pid.
	
fibonacci(N) when N=<1 -> 1;
fibonacci(N) -> fibonacci(N-1)+fibonacci(N-2).

factorial(N) when N=<0 -> 1;
factorial(N) -> N*factorial(N-1).

square(N) -> N*N.

loop() ->
	receive
    {Pid, fibonacci, X} ->
		io:format("Received fibonacci: ~w from ~w~n", [X, Pid]),
        Pid!fibonacci(X);
    {Pid, factorial, X} ->
		io:format("Received factorial: ~w from ~w~n", [X, Pid]),
        Pid!factorial(X);
    {Pid, square, X} ->
		io:format("Received square: ~w from ~w~n", [X, Pid]),
        Pid!square(X);
	X ->
		io:format("Received: ~w~n", [X])
	end,
	loop().

% Server = xpinterface_example:start().
% Server!{self(), fibonacci, 5}. receive X -> X after 1000 -> false end.
% Server!{self(), factorial, 5}. receive Y -> Y after 1000 -> false end.
% Server!{self(), square, 5}. receive Z -> Z after 1000 -> false end.
