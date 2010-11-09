
% marrower_test:test().
-module(marrower_test).

-compile(export_all).

-include("task_db.hrl").

test() ->
	marrower_server:start(),
	user_test(),
	task_test().
	
loop() ->
	receive X -> 
		io:format("received: ~p~n", [X])
	end,
	loop().

user_test() ->
	Listener = spawn(?MODULE, loop, []),
	io:format("list_all_users: ~p~n", 
		[marrower_server:list_all_users()]), 
	io:format("login: ~p~n", 
		[marrower_server:login("carlos", "Carlinhos", Listener)]), 
	io:format("login: ~p~n", 
		[marrower_server:login("chema", "Carlinhos", Listener)]), 
	io:format("list_all_users: ~p~n", 
		[marrower_server:list_all_users()]), 
	io:format("get_user_info: ~p~n", 
		[marrower_server:get_user_info("chema")]), 
	io:format("send_message: ~p~n", 
		[marrower_server:send_message("pepe", "carlos", "hola tio")]), 
	io:format("logout: ~p~n", 
		[marrower_server:logout("carlos")]), 
	io:format("list_all_users: ~p~n", 
		[marrower_server:list_all_users()]), 
	io:format("send_message: ~p~n", 
		[marrower_server:send_message("pepe", "carlos", "hola tio")]).
		
task_test() ->
	io:format("find_all_tasks: ~p~n", 
		[marrower_server:find_all_tasks()]), 
	io:format("find_task_by_id: ~p~n", 
		[marrower_server:find_task_by_id(15)]), 
	io:format("find_task_by_assigned: ~p~n", 
		[marrower_server:find_task_by_assigned("chema")]), 
		
	NewTask = 
		marrower_server:create_task(
			#task_info{
				name = "Test",
				description = "Test, test, test",
				duration = "20h",
				state = stoped, 
				completion = 0, 
				priority = low,
				assigned_to = ["chema"]}),

	io:format("find_task_by_id: ~p~n", 
		[marrower_server:find_task_by_id(NewTask#task_info.id)]), 
		
	io:format("update_task: ~p~n", 
		[marrower_server:update_task(
			NewTask#task_info{
				description = "Test2, test2, test2"}
			)]),
	io:format("remove_task: ~p~n", 
		[marrower_server:remove_task(
			NewTask#task_info.id)]),
	io:format("find_task_by_id: ~p~n", 
		[marrower_server:find_task_by_id(NewTask#task_info.id)]), 
	io:format("find_all_relations: ~p~n", 
		[marrower_server:find_all_relations()]), 
	io:format("find_predepends: ~p~n", 
		[marrower_server:find_predepends(15)]), 
	io:format("find_postdepends: ~p~n", 
		[marrower_server:find_postdepends(15)]), 
	io:format("add_relation: ~p~n", 
		[marrower_server:add_relation(15, 3)]), 
	io:format("remove_relation: ~p~n", 
		[marrower_server:remove_relation(15, 17)]), 
	io:format("find_postdepends: ~p~n", 
		[marrower_server:find_postdepends(15)]).
		
	
	
	
	
	
		
		
			
	
	
	
	