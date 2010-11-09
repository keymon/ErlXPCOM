-module(reply_server).
-export([start/0, loop/0, print/1, test_cnode_accept/1, proba_rpc/0]).

start() ->
	Pid=spawn(reply_server, loop, []),
	catch register(reply_server, Pid),
    Pid.

	
loop() ->
	receive
    {Pid, Msg} ->
		io:format("Received: ~w from ~w~n", [Msg, Pid]),
        Pid!Msg;
	X ->
		io:format("Received: ~w~n", [X])
	end,
	loop().

print(X) ->
	io:format("Print function: ~w~n",[X]),
    X.

test_reply_send(Term, Dest) ->
	io:format("Sending ~w to ~w~n",[Term, Dest]),
    Dest!{self(), Term},
    receive
		Term ->
			io:format("Received ~w, ok~n",[Term]),
			ok;
        Term2 ->
			io:format("Received ~w, fail~n",[Term2]),
            {fail, Term, Term2}
        after 1000 -> timeout
    end.

test_reply_send_list([], _) -> ok;
test_reply_send_list([H|T], Dest) ->
	case test_reply_send(H, Dest) of
        ok -> test_reply_send_list(T, Dest);
        X -> X
    end.

test_cnode_accept(Dest) ->
    Test_set =
		[an_atom, 3.14, 1234567890, <<1,2,3,4>>,
		 {}, {an_atom_in_a_tuple},  {an_atom_in_a_tuple, 123},
	   	 {{an_atom_in_a_tuple_in_a_tuple, 123}, 345},
         [], [an_atom_in_a_list], [1,2,3,4],
		 [1,2,3|an_atom_closing_a_list]],
    Ret = test_reply_send_list(Test_set, Dest),
    Dest!exit,
    Ret.

proba_rpc() ->
	receive after 1000 -> 1 end, 
	hola.

% c(reply_server).
% Server = reply_server:start().
% Server ! {self(), hola}.
% Server ! {self(), hola}.
% reply_server:test_cnode_accept(Server).
% reply_server:test_cnode_accept({reply_server, 'pepito@makako.local'}).
% {kk, 'pepito@KeySys.Ceibe.org'}! {self(), hola}. receive X -> X end.
% receive X -> X end.
% reply_server:imprime("hg").

% rex!{self(), {call, reply_server, print, ["hola"], user}}.
% {rex, 'pepita@KeySys.Ceibe.org'}!{self(), {call, reply_server, print, ["hola"], user}}
