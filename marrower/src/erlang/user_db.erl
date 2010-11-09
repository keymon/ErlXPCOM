%
% Funcións de xestión de usuarios
% 
-module(user_db).

-compile(export_all).

-include("user_db.hrl").

% BD de usuarios
-include("default_user_db.hrl").

init_db() ->
	get_registered_users().


find_all_users(Db) ->
	Fun = fun
		(_key, #user_info{state=UserState, login=Login, alias=Alias}, Acum) ->
			[{UserState, Login, Alias}|Acum]
		end,
	dict:fold(Fun, [], Db).

get_user_info(Login, Db) ->
	case dict:find(Login, Db) of 
		{ok, UserState} -> UserState;
		_ -> unknown_user
	end.

get_user_session(Login, Db) ->
	case dict:find(Login, Db) of 
		{ok, #user_info{state=offline}} ->
			offline;
		{ok, UserState} ->
			{UserState#user_info.state, 
			 UserState#user_info.pid};
		_ -> unknown_user
	end.

get_all_online_pids(Db) ->
	Fun = fun
		(_key, #user_info{state=online, pid=Pid}, Acum) -> [Pid|Acum];
		(_, _, Acum) -> Acum
		end,
	dict:fold(Fun, [], Db).

set_online(Login, Alias, ClientPid, Db) ->
	UpdateFun = 
		fun (UserState) ->
			UserState#user_info{
				state = online, 
				alias = Alias, 
				pid = ClientPid}
		end,
	case catch dict:update(Login, UpdateFun, Db) of
		{'EXIT', _} ->
			unknown_user;
		NewDict ->
			{ok, NewDict}
	end.
	
set_offline(Login, Db) ->
	UpdateFun = 
		fun (UserState) ->
			UserState#user_info{state=offline}
		end,
	case catch dict:update(Login, UpdateFun, Db) of
		{'EXIT', _} ->
			unknown_user;
		NewDict ->
			{ok, NewDict}
	end.

